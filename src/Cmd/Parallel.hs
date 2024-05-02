{-# LANGUAGE CPP, OverloadedStrings #-}

module Cmd.Parallel (
  parallelBuildCmd,
  ) where

import Common
import Common.System

import Control.Concurrent.Async
import Data.RPM.NVR (NVR)
import Distribution.RPM.Build.Order (dependencyLayersRpmOpts)
import Fedora.Bodhi hiding (bodhiUpdate)
import SimplePrompt (prompt, promptEnter, yesNo)
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
import RpmBuild (checkSourcesMatch, distRpmOptions, getDynSourcesMacros)
import Types

data JobDone = Done {_jobPkg :: Package,
                     jobNvr :: NVR,
                     jobBranch :: Branch,
                     _jobClog :: String}

type JobAsync = (String, Async JobDone)

-- FIXME offer to untag overrides afterwards
-- FIXME merge --from
-- FIXME print koji url of failed process or use koji-tool
-- FIXME option to build multiple packages over branches in parallel
-- FIXME use --wait-build=NVR
-- FIXME check sources as early as possible
-- FIXME --ignore-dependencies to build packages at once regardless
-- FIXME time builds
-- FIXME copy bodhi notes from another branch update
-- FIXME support non-sidetag update for parallel packages
-- FIXME print layers if few packages?
-- FIXME push update
parallelBuildCmd :: Bool -> Maybe Bool -> Int -> Maybe SideTagTarget -> Double
                 -> (Maybe UpdateType, UpdateSeverity)
                 -> (BranchesReq, [String]) -> IO ()
parallelBuildCmd dryrun mmerge firstlayer msidetagTarget delay mupdate (breq, pkgs) =
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
             _ -> listOfBranches True True breq
  when (null branches) $
    error' "Please specify at least one branch"
  when (isJust (maybeTarget msidetagTarget) && length branches > 1) $
    error' "You can only specify target with one branch"
  case pkgs of
    [] -> getPackageName "." >>= parallelBranches branches
    [p] -> withExistingDirectory p $
           getPackageName p >>= parallelBranches branches
    _ ->
      forM_ branches $ \rbr -> do
      mtargetSidetag <-
        if msidetagTarget == Just SideTag
        then do
          tags <- map (head . words) <$> kojiUserSideTags (Just rbr)
          case tags of
            [] -> do
              putStrLn "will use new sidetag"
              return $ Just SideTag
            [tag] -> do
              putStrLn $ "will use" +-+ tag
              return $ Just (Target tag)
            _ -> error' $ "multiple existing sidetags:" +-+ show tags
        else return msidetagTarget
      forM_ pkgs $ \p ->
        when (mmerge /= Just False) $
        withExistingDirectory p $ do
        pkg <- getPackageName p
        mergeNewerBranch (Just pkg) rbr
        getDynSourcesMacros $ packageSpec pkg
      distopts <- distRpmOptions rbr
      allLayers <- dependencyLayersRpmOpts distopts pkgs
      let layers = drop firstlayer allLayers
      when (isNothing msidetagTarget && length allLayers > 1) $
        unlessM (checkAutoBodhiUpdate rbr) $
          error' "You must use --target/--sidetag to build package layers for this branch"
      when (length branches > 1) $
        putStrLn $ "#" +-+ show rbr
      target <- targetMaybeSidetag dryrun True rbr mtargetSidetag
      nvrclogs <- concatMapM (timeIO . parallelBuild target rbr)
                      (zip [firstlayer..length allLayers] $
                       init $ tails layers) -- tails ends in []
      unless (isNothing (fst mupdate)) $
        unless (isNothing msidetagTarget) $
        -- FIXME check for an existing sidetag update
        when (target /= branchTarget rbr) $ do
        let changelog = intercalate "" $ renderChangelogs $ reverse nvrclogs
        putNewLn
        putStrLn changelog
        when (length changelog > 10000) $
          putStrLn "Bodhi only accepts up to 10000 chars: will be truncated"
        -- FIXME allow editor
        input <- prompt "Press Enter to submit above or input Bodhi update summary now; or 'no' to skip update"
        unless (trim (lower input) `elem` ["no","n"] || dryrun) $
          bodhiSidetagUpdate rbr (map jobNvr nvrclogs) target $
          if null input then changelog else input
  where
    parallelBranches :: [Branch] -> Package -> IO ()
    parallelBranches brs pkg = do
      krbTicket
      currentbranch <- gitCurrentBranch
      putStrLn $ "= Building" +-+ pluralException (length brs) "branch" "branches" +-+ "in parallel:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      (failures,nvrclogs) <- timeIO $ watchJobs (length jobs == 1) Nothing [] [] jobs
      -- switch back to the original branch
      when (length brs /= 1) $
        gitSwitchBranch currentbranch
      unless (null failures) $ do
        putStrLn $ "Build failures:" +-+ unwords failures
        okay <- yesNo "Do you want to continue nevertheless"
        unless okay $ error' "Quitting"
      when (isNothing msidetagTarget) $ do
        let spec = packageSpec pkg
        bodhiUpdate dryrun mupdate Nothing False spec $
          intercalate "," . map (showNVR . jobNvr) $
          filter ((/= Rawhide) . jobBranch) nvrclogs
      where
        -- FIXME time jobs
        setupBranch :: Branch -> IO JobAsync
        setupBranch br = do
          putPkgBrnchHdr pkg br
          target <- targetMaybeSidetag dryrun True br msidetagTarget
          when (mmerge /= Just False) $ mergeNewerBranch Nothing br
          job <- startBuild Nothing 0 False (length brs) target pkg br "." >>= async
          unless dryrun $ sleep delay
          return (show br,job)

    mergeNewerBranch :: Maybe Package -> Branch -> IO ()
    mergeNewerBranch mpkg br = do
      gitSwitchBranch (RelBranch br)
      (ancestor,unmerged,mnewer) <- newerMergeable br
      unless dryrun $
        whenJust mnewer $ \newer ->
        mergeBranch dryrun False (mmerge == Just True) False mpkg (ancestor,unmerged) newer br

    -- FIXME time builds or layers
    parallelBuild :: String -> Branch -> (Int,[[String]])
                  -> IO [JobDone]
    parallelBuild _ _ (_,[]) = return [] -- should not reach here
    parallelBuild target br (layernum, layer:nextLayers) =  do
      krbTicket
      let singlelayer = layernum == 0 && null nextLayers
      putStrLn $ "\n= Building" +-+
        (if singlelayer
         then "in parallel"
         else (if null nextLayers && not singlelayer then "final" else "") +-+ "parallel layer #" ++ show layernum) ++
        if nopkgs > 1
        then " (" ++ show nopkgs +-+ "packages):"
        else ":"
      putStrLn $ unwords layer
      -- maybe print total pending packages
      unless (null nextLayers) $
        putStrLn $
        let layerspkgs = map length nextLayers
        in case layerspkgs of
          [n] -> plural n "more package" +-+ "left in next final layer"
          _ -> plural (length layerspkgs) "more package layer" +-+ "left:" +-+
               show layerspkgs
      jobs <- zipWithM (setupBuild singlelayer) (reverse [0..(length layer - 1)]) layer
      when (null jobs) $
        error' "No jobs run"
      (failures,nvrs) <- watchJobs (length jobs == 1) (if singlelayer then Nothing else Just layernum) [] [] jobs
      if null failures
        then return nvrs
        else do
        let pending = sum $ map length nextLayers
        putStrLn $ "\nBuild failures" +-+
          (if singlelayer then ":" else "in layer" +-+ show layernum ++ ":")
          +-+ unwords failures
        okay <-
          if null nextLayers
          then return False
          else yesNo "Do you want to continue nevertheless"
        if okay
          then return nvrs
          else error' $
               plural pending "pending package" ++
               if pending > 0
               then ":\n" ++ unwords (map unwords nextLayers)
               else ""
      where
        nopkgs = length layer
        layersleft = length nextLayers

        setupBuild :: Bool -> Int -> String -> IO JobAsync
        setupBuild singlelayer n dir = do
          pkg <- getPackageName dir
          putPkgBrnchHdr pkg br
          job <- startBuild (if singlelayer then Nothing else Just layernum) n (layersleft > 0) nopkgs target pkg br dir
                 >>= async
          unless dryrun $ sleep delay
          return (unPackage pkg,job)

    watchJobs :: Bool -> Maybe Int -> [String] -> [JobDone] -> [JobAsync]
              -> IO ([String],[JobDone]) -- (failures,successes)
    watchJobs _ _ fails dones [] = return (fails,dones)
    watchJobs singlejob mlayer fails dones (job:jobs) = do
      status <- poll (snd job)
      case status of
        Nothing -> sleep 1 >> watchJobs singlejob mlayer fails dones (jobs ++ [job])
        -- (nvr,changelog)
        Just (Right done) -> do
          unless singlejob $
            when (null jobs) $
            putStrLn $ "ending" +-+ maybe "" (\l -> "layer" +-+ show l) mlayer
          watchJobs singlejob mlayer fails (done:dones) jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "**" +-+ color Magenta pkg +-+ "job" +-+ color Magenta "failed" +-+ "**" +-+ if singlejob then "" else "(" ++ plural (length jobs) "job" +-+ "left" +-+ maybe "" (\l ->  "in layer" +-+ show l) mlayer ++ ")"
          watchJobs singlejob mlayer (pkg : fails) dones jobs

    -- FIXME prefix output with package name
    startBuild :: Maybe Int -> Int ->  Bool -> Int -> String -> Package
               -> Branch -> String -> IO (IO JobDone)
    startBuild mlayer n morelayers nopkgs target pkg br dir =
      withExistingDirectory dir $ do
      gitSwitchBranch (RelBranch br)
      unlessM isGitDirClean $
        error' "local uncommitted changes (dirty)"
      let spec = packageSpec pkg
      checkForSpecFile spec
      nvr <- pkgNameVerRel' br spec
      unpushed <- gitOneLineLog $ "origin/" ++ show br ++ "..HEAD"
      unless (null unpushed) $ do
        putStrLn $ showNVR nvr +-+ "(" ++ target ++ ")" +-+
          if nopkgs > 1
          then pluralException n "more" "more" +-+ maybe "" (\l -> "in layer" +-+ show l) mlayer
          else ""
        putNewLn
        displayCommits True unpushed
      unless (null unpushed) $ do
        checkSourcesMatch pkg (RelBranch br) spec
        unless dryrun $
          gitPush True Nothing
      changelog <- unlines <$> getChangelog spec
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = if target == branchTarget br then branchDestTag br else target
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          -- FIXME detect old stable existing build
          putStrLn $ color Green (showNVR nvr) +-+ "is already" +-+ color Green "built"
          when (br /= Rawhide && morelayers && target == branchTarget br) $ do
            tags <- kojiNVRTags nvr
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
              unlessM (checkAutoBodhiUpdate br) $
              bodhiCreateOverride dryrun Nothing nvr
          return $ do
            when morelayers $
              kojiWaitRepo dryrun (nopkgs > 5) True target nvr
            return $ Done pkg nvr br changelog
        Just BuildBuilding -> do
          putStrLn $ color Yellow (showNVR nvr) +-+ "is already" +-+ color Yellow "building"
          mtask <- kojiGetBuildTaskID fedoraHub $ showNVR nvr
          case mtask of
            Nothing -> error' $ "Task for" +-+ showNVR nvr +-+ "not found"
            Just task ->
              return $ do
              kojiWaitTaskAndRepo (isNothing mlatest) nvr task
              return $ Done pkg nvr br changelog
        _ -> do
          when (null unpushed) $ do
            putStrLn $ showNVR nvr +-+ "(" ++ target ++ ")" +-+ show n +-+ "more" +-+
              maybe "" (\l -> "in layer" +-+ show l) mlayer
            putNewLn
            putStrLn changelog
          buildref <- git "show-ref" ["--hash", "origin/" ++ show br]
          opentasks <- kojiOpenTasks pkg (Just buildref) target
          case opentasks of
            [task] -> do
              putStrLn $ showNVR nvr +-+ "task is already open"
              return $ do
                kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                return $ Done pkg nvr br changelog
            (_:_) -> error' $ show (length opentasks) +-+ "open" +-+ unPackage pkg +-+ "tasks already"
            [] -> do
              if equivNVR nvr mlatest
                then do
                -- FIXME add a retry prompt
                putStrLn $ color Red $ showNVR nvr +-+ "is already latest (modulo disttag)"
                return $ error' $ showNVR nvr +-+ "failed"
                else do
                -- FIXME parse build output
                if dryrun
                  then return $ return $ Done pkg nvr br "<changelog>"
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing $ "--fail-fast" : ["--background" | nopkgs > 5]
                  return $ do
                    kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                    return $ Done pkg nvr br changelog
      where
        kojiWaitTaskAndRepo :: Bool -> NVR -> TaskID -> IO ()
        kojiWaitTaskAndRepo newpkg nvr task = do
          finish <- kojiWaitTask task
          if finish
            then putStrLn $ color Green $ showNVR nvr +-+ "build success"
            -- FIXME print koji task url
            else do
            whenJustM (findExecutable "koji-tool") $ \kojitool ->
              -- FIXME cmdLog deprecated
              -- FIXME use --children (koji-tool-1.1.2)
              cmdLog kojitool ["tasks", displayID task, "-s", "fail"]
            error' $ color Red $ showNVR nvr +-+ "build failed"
          autoupdate <- checkAutoBodhiUpdate br
          if autoupdate then
            when newpkg $ do
            mBugSess <- bzReviewSession
            whenJust mBugSess $ \(bid,session) ->
              putBugBuild dryrun session bid nvr
            else do
            when (target == branchTarget br && morelayers) $
              -- -- FIXME: avoid prompt in
              -- changelog <- changeLogPrompt False spec
              -- bodhiUpdate (fmap fst mBugSess) changelog nvr
              bodhiCreateOverride dryrun Nothing nvr
          when morelayers $
            kojiWaitRepo dryrun (nopkgs > 5) True target nvr

    -- FIXME map nvr to package?
    renderChangelogs :: [JobDone] -> [String]
    renderChangelogs [] = []
    renderChangelogs ((Done pkg _ _ clog):jobs) =
      unlines [unPackage pkg ++ ":", "", clog] : renderChangelogs jobs

    -- FIXME how to catch authentication errors?
    bodhiSidetagUpdate :: Branch -> [NVR] -> String -> String -> IO ()
    bodhiSidetagUpdate rbr nvrs sidetag notes = do
      case mupdate of
        (Nothing, _) -> return ()
        (Just updateType, severity) -> do
          putStrLn $ "Creating Bodhi Update for" +-+ sidetag
          ok <-
            if updateType == TemplateUpdate
            then do
              putStrLn "Paste update template now:"
              template <- getContents
              cmdBool "bodhi" ["updates", "new", "--file", template, "--from-tag", sidetag]
            else cmdBool "bodhi" ["updates", "new", "--type", show updateType , "--severity", show severity, "--request", "testing", "--notes", if null notes then "to be written" else take 10000 notes, "--autokarma", "--autotime", "--close-bugs", "--from-tag", sidetag]
          if not ok
            then bodhiSidetagUpdate rbr nvrs sidetag notes
            else
            unlessM (checkAutoBodhiUpdate rbr) $ do
            -- FIXME get updateid from above bodhi command output
            res <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" (showNVR (last nvrs))]
            case res of
              [] -> do
                putStrLn "bodhi submission failed"
                promptEnter "Press Enter to resubmit to Bodhi"
                bodhiSidetagUpdate rbr nvrs sidetag notes
              [update] ->
                case lookupKey "updateid" update :: Maybe String of
                  Nothing -> error' "could not determine Update id"
                  Just _updateid -> return ()
              _ -> error' $ "impossible happened: more than one update found for" +-+ showNVR (last nvrs)
