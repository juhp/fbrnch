{-# LANGUAGE OverloadedStrings #-}

module Cmd.Parallel (
  parallelBuildCmd,
  SideTagTarget(..)
  ) where

import Common
import Common.System

import Control.Concurrent.Async
import Distribution.RPM.Build.Order (dependencyLayers)
import Fedora.Bodhi
import System.Console.Pretty
import System.Time.Extra (sleep)

import Bodhi
import Bugzilla
import Branches
import Cmd.Merge (mergeBranch)
import Git
import Krb
import Koji
import Package
import Prompt

data SideTagTarget = SideTag | Target String

maybeTarget :: Maybe SideTagTarget -> Maybe String
maybeTarget (Just (Target t)) = Just t
maybeTarget _ = Nothing

-- (pkg, nvr)
type Job = (String, Async (String,String))

-- FIXME print koji url of failed process or use koji-tool
-- FIXME option to build multiple packages over branches in parallel
-- FIXME use --wait-build=NVR
-- FIXME check sources as early as possible
-- FIXME --single-layer to build packages at once regardless
-- FIXME time builds
-- FIXME copy bodhi notes from another branch update
parallelBuildCmd :: Bool -> Maybe Bool -> Int -> Maybe SideTagTarget
                 -> (Maybe UpdateType, UpdateSeverity)
                 -> (BranchesReq, [String]) -> IO ()
parallelBuildCmd dryrun mmerge firstlayer msidetagTarget mupdate (breq, pkgs) =
  do
  branches <-
    case pkgs of
      [] -> do
        unlessM isPkgGitSshRepo $
          error' "Please specify at least one package"
        listOfBranches True True breq
      [p] -> withExistingDirectory p $ listOfBranches True True breq
      _ -> case breq of
             Branches [] -> error' "please specify a branch"
             Branches _ -> listOfBranches True True breq
             _ -> listOfBranches False True breq
  when (null branches) $
    error' "Please specify at least one branch"
  let mtarget = maybeTarget msidetagTarget
  when (isJust mtarget && length branches > 1) $
    error' "You can only specify target with one branch"
  case pkgs of
    [] -> timeIO $ parallelBranches branches
    [p] -> withExistingDirectory p $
           parallelBranches branches
    _ ->
      forM_ branches $ \ rbr -> do
      forM_ pkgs $ \p ->
        withExistingDirectory p $ mergeNewerBranch p rbr
      allLayers <- dependencyLayers pkgs
      let layers = drop firstlayer allLayers
      when (isNothing msidetagTarget && length allLayers > 1) $
        unlessM (checkAutoBodhiUpdate rbr) $
          error' "You must use --target/--sidetag to build package layers for this branch"
      when (length branches > 1) $
        putStrLn $ "# " ++ show rbr
      target <- targetMaybeSidetag rbr
      nvrclogs <- concatMapM (timeIO . parallelBuild target rbr)
                      (zip [firstlayer..length allLayers] $
                       init $ tails layers) -- tails ends in []
      unless (isNothing msidetagTarget || dryrun) $ do
        when (target /= branchTarget rbr) $ do
          let changelog = intercalate "" $ renderChangelogs nvrclogs
          putStrLn ""
          putStrLn changelog
          input <- prompt "Press Enter to use above or input update summary now; or 'no' to skip update"
          unless (trim (lower input) == "no") $
            bodhiSidetagUpdate rbr (map fst nvrclogs) target $
            if null input then changelog else input
  where
    parallelBranches :: [Branch] -> IO ()
    parallelBranches brs = do
      krbTicket
      currentbranch <- gitCurrentBranch
      putStrLn $ "= Building " ++ pluralException (length brs) "branch" "branches" ++ " in parallel:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      (failures,_nvrs) <- watchJobs [] [] jobs
      -- switch back to the original branch
      when (length brs /= 1) $
        gitSwitchBranch currentbranch
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      where
        -- FIXME time jobs
        setupBranch :: Branch -> IO Job
        setupBranch br = do
          target <- targetMaybeSidetag br
          when (mmerge /= Just False) $ mergeNewerBranch (show br) br
          job <- startBuild False False target br "." >>= async
          unless dryrun $ sleep 3
          return (show br,job)

    mergeNewerBranch :: String -> Branch -> IO ()
    mergeNewerBranch desc br = do
      gitSwitchBranch (RelBranch br)
      (ancestor,unmerged) <- newerMergeable br
      newer <- getNewerBranch br
      when (ancestor && not (null unmerged)) $
        putStrLn $ "Checking " ++ desc ++ ":"
      mergeBranch True (mmerge == Just True) (ancestor,unmerged) newer br

    -- FIXME time builds or layers
    parallelBuild :: String -> Branch -> (Int,[[String]])
                  -> IO [(String,String)]
    parallelBuild _ _ (_,[]) = return [] -- should not reach here
    parallelBuild target br (layernum, layer:nextLayers) =  do
      krbTicket
      putStrLn $ "\n= Building parallel layer #" ++ show layernum ++
        if nopkgs > 1
        then " (" ++ show nopkgs ++ " packages):"
        else ":"
      putStrLn $ unwords layer
      -- maybe print total pending packages
      unless (null nextLayers) $
        putStrLn $ plural layersleft "more layer" ++ " left with " ++
        let layerspkgs = map length nextLayers
        in case layerspkgs of
             [l] -> plural l "package"
             _ -> show layerspkgs ++ " packages"
      jobs <- mapM setupBuild layer
      when (null jobs) $
        error' "No jobs run"
      (failures,nvrs) <- watchJobs [] [] jobs
      -- FIXME prompt to continue?
      if null failures
        then return nvrs
        else do
        let pending = sum $ map length nextLayers
        error' $ "Build failures in layer " ++ show layernum ++ ": " ++
          unwords failures ++ "\n\n" ++
          show pending ++ " pending packages" ++
          if pending > 0
          then
          ":\n" ++ unwords (map unwords nextLayers)
          else ""
      where
        nopkgs = length layer
        layersleft = length nextLayers

        setupBuild :: String -> IO Job
        setupBuild pkg = do
          job <- startBuild (layersleft > 0) (nopkgs > 5) target br pkg
                 >>= async
          unless dryrun $ sleep 3
          return (pkg,job)

    -- (failures,successes)
    watchJobs :: [String] -> [(String,String)] -> [Job]
              -> IO ([String],[(String,String)])
    watchJobs fails results [] = return (fails,results)
    watchJobs fails results (job:jobs) = do
      status <- poll (snd job)
      case status of
        Nothing -> sleep 1 >> watchJobs fails results (jobs ++ [job])
        -- (nvr,changelog)
        Just (Right result) -> do
          putStrLn $ color Yellow (fst result) ++ " job completed (" ++ show (length jobs) ++ " left in layer)"
          watchJobs fails (result:results) jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "** " ++ color Magenta pkg ++ " job " ++ color Magenta "failed" ++ " ** (" ++ show (length jobs) ++ " left in layer)"
          watchJobs (pkg : fails) results jobs

    -- FIXME prefix output with package name
    startBuild :: Bool -> Bool -> String -> Branch -> String
               -> IO (IO (String,String))
    startBuild morelayers background target br pkgdir =
      withExistingDirectory pkgdir $ do
      gitSwitchBranch (RelBranch br)
      pkg <- getPackageName pkgdir
      putPkgBrnchHdr pkg br
      unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
      unless (null unpushed) $
        mapM_ putStrLn unpushed
      let spec = packageSpec pkg
      checkForSpecFile spec
      unless (null unpushed) $ do
        checkSourcesMatch spec
        unless dryrun $
          gitPushSilent Nothing
      nvr <- pkgNameVerRel' br spec
      putStrLn $ nvr ++ " (" ++ target ++ ")"
      changelog <- unlines <$> getChangelog spec
      putStrLn changelog
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = if target == branchTarget br then branchDestTag br else target
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          -- FIXME detect old stable existing build
          putStrLn $ color Green nvr ++ " is " ++ color Green "already built"
          when (br /= Rawhide && morelayers && target == branchTarget br) $ do
            tags <- kojiNVRTags nvr
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
              unlessM (checkAutoBodhiUpdate br) $
              bodhiCreateOverride dryrun Nothing nvr
          return $ do
            when morelayers $
              kojiWaitRepo dryrun target nvr
            return (nvr,changelog)
        Just BuildBuilding -> do
          putStrLn $ nvr ++ " is already building"
          mtask <- kojiGetBuildTaskID fedoraHub nvr
          case mtask of
            Nothing -> error' $ "Task for " ++ nvr ++ " not found"
            Just task ->
              return $ do
              kojiWaitTaskAndRepo (isNothing mlatest) nvr task
              return (nvr,changelog)
        _ -> do
          buildref <- git "show-ref" ["--hash", "origin/" ++ show br]
          opentasks <- kojiOpenTasks pkg (Just buildref) target
          case opentasks of
            [task] -> do
              putStrLn $ nvr ++ " task is already open"
              return $ do
                kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                return (nvr,changelog)
            (_:_) -> error' $ show (length opentasks) ++ " open " ++ unPackage pkg ++ " tasks already"
            [] -> do
              if equivNVR nvr (fromMaybe "" mlatest)
                then return $ error' $
                     color Red $ nvr ++ " is already latest (modulo disttag)"
                else do
                -- FIXME parse build output
                if dryrun
                  then return $ return (nvr,"<changelog>")
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing $ "--fail-fast" : ["--background" | background]
                  return $ do
                    kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                    return (nvr,changelog)
      where
        kojiWaitTaskAndRepo :: Bool -> String -> TaskID -> IO ()
        kojiWaitTaskAndRepo newpkg nvr task = do
          finish <- kojiWaitTask task
          if finish
            then putStrLn $ color Green $ nvr ++ " build success"
            -- FIXME print koji task url
            else error' $ color Red $ nvr ++ " build failed"
          autoupdate <- checkAutoBodhiUpdate br
          if autoupdate then
            when newpkg $ do
            mBugSess <- bzReviewSession
            whenJust mBugSess $ \(bid,session) ->
              putBugBuild dryrun session bid nvr
            else do
            when (target == branchTarget br && morelayers) $
              -- -- FIXME: avoid prompt in
              -- changelog <- changeLogPrompt Nothing spec
              -- bodhiUpdate (fmap fst mBugSess) changelog nvr
              bodhiCreateOverride dryrun Nothing nvr
          when morelayers $
            kojiWaitRepo dryrun target nvr

    -- FIXME map nvr to package?
    renderChangelogs :: [(String,String)] -> [String]
    renderChangelogs [] = []
    renderChangelogs ((nvr,clog):nvrclogs) =
      unlines [nvr, "", clog] : renderChangelogs nvrclogs

    -- FIXME how to catch authentication errors?
    bodhiSidetagUpdate :: Branch -> [String] -> String -> String -> IO ()
    bodhiSidetagUpdate rbr nvrs sidetag notes = do
      case mupdate of
        (Nothing, _) -> return ()
        (Just updateType, severity) -> do
          putStrLn $ "Creating Bodhi Update for " ++ sidetag
          ok <-
            if updateType == TemplateUpdate
            then do
              putStrLn "Paste update template now:"
              template <- getContents
              cmdBool "bodhi" ["updates", "new", "--file", template, "--from-tag", sidetag]
            else cmdBool "bodhi" ["updates", "new", "--type", show updateType , "--severity", show severity, "--request", "testing", "--notes", if null notes then "to be written" else notes, "--autokarma", "--autotime", "--close-bugs", "--from-tag", sidetag]
          if not ok
            then bodhiSidetagUpdate rbr nvrs sidetag notes
            else
            unlessM (checkAutoBodhiUpdate rbr) $ do
            prompt_ "Press Enter to remove the sidetag"
            fedpkg_ "remove-side-tag" [sidetag]
            -- arguably we already received the Updateid from the above bodhi
              -- command, but we query it here via nvr
            prompt_ "Press Enter to edit update just to unlock it from sidetag"
            res <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" (last nvrs)]
            case res of
              [] -> do
                putStrLn "bodhi submission failed"
                prompt_ "Press Enter to resubmit to Bodhi"
                bodhiSidetagUpdate rbr nvrs sidetag notes
              [update] ->
                case lookupKey "updateid" update of
                  Nothing -> error' "could not determine Update id"
                  Just updateid -> do
                    -- disconnect the update from the sidetag
                    -- so it can be changed after sidetag closed
                    -- see https://github.com/fedora-infra/bodhi/issues/4563 for the auto options
                    cmd_ "bodhi" ["updates", "edit", updateid, "--autokarma", "--autotime"]
                    putStrLn "Update edited to unlock from sidetag"
              _ -> error' $ "impossible happened: more than one update found for " ++ last nvrs

    targetMaybeSidetag :: Branch -> IO String
    targetMaybeSidetag br =
      case msidetagTarget of
        Nothing -> return $ branchTarget br
        Just (Target t) -> return t
        Just SideTag -> do
          tags <- map (head . words) <$> kojiUserSideTags (Just br)
          case tags of
            [] -> do
              Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub (show br)
              out <- head . lines <$> fedpkg "request-side-tag" ["--base-tag",  buildtag]
              if "Side tag '" `isPrefixOf` out
                then do
                putStrLn out
                let sidetag =
                      init . dropWhileEnd (/= '\'') $ dropPrefix "Side tag '" out
                putStrLn $ "waiting for " ++ sidetag ++ " repo"
                cmd_ "koji" ["wait-repo", sidetag]
                return sidetag
                else error' "'fedpkg request-side-tag' failed"
            [tag] -> return tag
            _ -> error' $ "More than one user side-tag found for " ++ show br
