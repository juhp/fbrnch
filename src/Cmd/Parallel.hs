{-# LANGUAGE OverloadedStrings #-}

module Cmd.Parallel (
  parallelBuildCmd,
  SideTagTarget(..)
  ) where

import Common
import Common.System

import Control.Concurrent.Async
import Distribution.RPM.Build.Order (dependencyLayers)
import System.Console.Pretty
import System.Time.Extra (sleep)

import Bodhi
import Bugzilla
import Branches
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
type Job = (String, Async String)

-- FIXME print koji url of failed process or use koji-tool
-- FIXME option to build multiple packages over branches in parallel
-- FIXME use --wait-build=NVR
-- FIXME check sources as early as possible
-- FIXME --single-layer to build packages at once regardless
-- FIXME time builds
parallelBuildCmd :: Bool -> Int -> Maybe SideTagTarget
                 -> (Maybe UpdateType, UpdateSeverity)
                 -> (BranchesReq, [String]) -> IO ()
parallelBuildCmd dryrun firstlayer msidetagTarget mupdate (breq, pkgs) = do
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
             _ -> error' "parallel does not support branch options for multiple packages: please give an explicit list of branches instead"
  when (null branches) $
    error' "Please specify at least one branch"
  let mtarget = maybeTarget msidetagTarget
  when (isJust mtarget && length branches > 1) $
    error' "You can only specify target with one branch"
  case pkgs of
    [] -> parallelBranches branches
    [p] -> withExistingDirectory p $
           parallelBranches branches
    _ ->
      forM_ branches $ \ rbr -> do
      allLayers <- dependencyLayers pkgs
      let layers = drop firstlayer allLayers
      when (isNothing msidetagTarget && length allLayers > 1) $
        unlessM (checkAutoBodhiUpdate rbr) $
          error' "You must use --target/--sidetag to build package layers for this branch"
      when (length branches > 1) $
        putStrLn $ "# " ++ show rbr
      target <- targetMaybeSidetag rbr
      mapM_ (parallelBuild target rbr)
                 $ zip [firstlayer..length allLayers]
                 $ init $ tails layers -- tails ends in []
      unless (isNothing msidetagTarget || dryrun) $ do
        when (target /= branchTarget rbr) $ do
          notes <- prompt $ "Enter notes to submit Bodhi update for " ++ target
          bodhiSidetagUpdate target notes
  where
    parallelBranches :: [Branch] -> IO ()
    parallelBranches brs = do
      krbTicket
      currentbranch <- gitCurrentBranch
      putStrLn $ "= Building " ++ pluralException (length brs) "branch" "branches" ++ " in parallel:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      failures <- watchJobs [] jobs
      -- switch back to the original branch
      when (length brs /= 1) $
        gitSwitchBranch currentbranch
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      where
        setupBranch :: Branch -> IO Job
        setupBranch br = do
          target <- targetMaybeSidetag br
          job <- startBuild False False target br "." >>= async
          unless dryrun $ sleep 3
          return (show br,job)

    parallelBuild :: String -> Branch -> (Int,[[String]]) -> IO ()
    parallelBuild _ _ (_,[]) = return () -- should not reach here
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
      failures <- watchJobs [] jobs
      -- FIXME prompt to continue?
      unless (null failures) $ do
        let pending = sum $ map length nextLayers
        error' $ "Build failures in layer " ++ show layernum ++ ": " ++
          unwords failures ++ "\n\n" ++
          show pending ++ " pending packages" ++
          if pending > 0
          then
          ":\n" ++ unwords (map unwords nextLayers)
          else ""
      when (null jobs) $
        error' "No jobs run"
      where
        nopkgs = length layer
        layersleft = length nextLayers

        setupBuild :: String -> IO Job
        setupBuild pkg = do
          job <- startBuild (layersleft > 0) (nopkgs > 5) target br pkg
                 >>= async
          unless dryrun $ sleep 3
          return (pkg,job)

    watchJobs :: [String] -> [Job] -> IO [String]
    watchJobs fails [] = return fails
    watchJobs fails (job:jobs) = do
      status <- poll (snd job)
      case status of
        Nothing -> sleep 1 >> watchJobs fails (jobs ++ [job])
        Just (Right nvr) -> do
          putStrLn $ color Yellow nvr ++ " job completed (" ++ show (length jobs) ++ " left in layer)"
          watchJobs fails jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "** " ++ color Magenta pkg ++ " job " ++ color Magenta "failed" ++ " ** (" ++ show (length jobs) ++ " left in layer)"
          watchJobs (pkg : fails) jobs

    -- FIXME prefix output with package name
    startBuild :: Bool -> Bool -> String -> Branch -> String -> IO (IO String)
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
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = if target == branchTarget br then branchDestTag br else target
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          putStrLn $ color Green nvr ++ " is " ++ color Green "already built"
          when (br /= Rawhide && morelayers && target == branchTarget br) $ do
            tags <- kojiNVRTags nvr
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
              unlessM (checkAutoBodhiUpdate br) $
              bodhiCreateOverride dryrun Nothing nvr
          return $ do
            when morelayers $
              kojiWaitRepo dryrun target nvr
            return nvr
        Just BuildBuilding -> do
          putStrLn $ nvr ++ " is already building"
          return $
            kojiGetBuildTaskID fedoraHub nvr >>=
            maybe (error' $ "Task for " ++ nvr ++ " not found")
            (kojiWaitTaskAndRepo (isNothing mlatest) nvr)
        _ -> do
          buildref <- git "show-ref" ["--hash", "origin/" ++ show br]
          opentasks <- kojiOpenTasks pkg (Just buildref) target
          case opentasks of
            [task] -> do
              putStrLn $ nvr ++ " task is already open"
              return $ kojiWaitTaskAndRepo (isNothing mlatest) nvr task
            (_:_) -> error' $ show (length opentasks) ++ " open " ++ unPackage pkg ++ " tasks already"
            [] -> do
              if equivNVR nvr (fromMaybe "" mlatest)
                then return $ error' $ color Red $ nvr ++ " is already latest (modulo disttag)"
                else do
                -- FIXME parse build output
                if dryrun
                  then return (return nvr)
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing $ "--fail-fast" : ["--background" | background]
                  return $ kojiWaitTaskAndRepo (isNothing mlatest) nvr task
      where
        kojiWaitTaskAndRepo :: Bool -> String -> TaskID -> IO String
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
          return nvr

    bodhiSidetagUpdate :: String -> String -> IO ()
    bodhiSidetagUpdate sidetag notes = do
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
          when ok $ do
            prompt_ "After editing update, press Enter to remove sidetag"
            fedpkg_ "remove-side-tag" [sidetag]

    targetMaybeSidetag :: Branch -> IO String
    targetMaybeSidetag br =
      case msidetagTarget of
        Nothing -> return $ branchTarget br
        Just (Target t) -> return t
        Just SideTag -> do
          tags <- map (head . words) <$> kojiUserSideTags (Just br)
          case tags of
            [] -> do
              out <- head . lines <$> fedpkg "request-side-tag" ["--base-tag",  show br ++ "-build"]
              if "Side tag '" `isPrefixOf` out
                then do
                putStrLn out
                return $ init . dropWhileEnd (/= '\'') $ dropPrefix "Side tag '" out
                else error' "'fedpkg request-side-tag' failed"
            [tag] -> return tag
            _ -> error' $ "More than one user side-tag found for " ++ show br
