{-# LANGUAGE CPP, OverloadedStrings #-}

module Cmd.Parallel (
  parallelBuildCmd,
  ) where

import Common
import Common.System

import Control.Concurrent.Async
import Data.Aeson (Object,Value(String))
import qualified Data.Text as T
import Distribution.RPM.Build.Order (dependencyLayers)
import Fedora.Bodhi hiding (bodhiUpdate)
import qualified Fedora.Bodhi as FedoraBodhi (bodhiUpdate)
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
import RpmBuild (checkSourcesMatch)
import Prompt
import Types

data JobDone = JobDone {jobNvr :: String,
                        jobBranch :: Branch,
                        _jobClog :: String
                       }
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
    [] -> getPackageName "." >>= parallelBranches branches
    [p] -> withExistingDirectory p $
           getPackageName p >>= parallelBranches branches
    _ ->
      forM_ branches $ \rbr -> do
      forM_ pkgs $ \p ->
        when (mmerge /= Just False) $
        withExistingDirectory p $ do
        pkg <- getPackageName p
        mergeNewerBranch (Just pkg) rbr
      allLayers <- dependencyLayers pkgs
      let layers = drop firstlayer allLayers
      when (isNothing msidetagTarget && length allLayers > 1) $
        unlessM (checkAutoBodhiUpdate rbr) $
          error' "You must use --target/--sidetag to build package layers for this branch"
      when (length branches > 1) $
        putStrLn $ "# " ++ show rbr
      target <- targetMaybeSidetag dryrun rbr msidetagTarget
      nvrclogs <- concatMapM (timeIO . parallelBuild target rbr)
                      (zip [firstlayer..length allLayers] $
                       init $ tails layers) -- tails ends in []
      unless (isNothing (fst mupdate)) $
        unless (isNothing msidetagTarget) $
        -- FIXME check for an existing sidetag update
        when (target /= branchTarget rbr) $ do
        let changelog = intercalate "" $ renderChangelogs nvrclogs
        putNewLn
        putStrLn changelog
        input <- prompt "Press Enter to use above or input update summary now; or 'no' to skip update"
        unless (trim (lower input) `elem` ["no","n"] || dryrun) $
          bodhiSidetagUpdate rbr (map jobNvr nvrclogs) target $
          if null input then changelog else input
  where
    parallelBranches :: [Branch] -> Package -> IO ()
    parallelBranches brs pkg = do
      krbTicket
      currentbranch <- gitCurrentBranch
      putStrLn $ "= Building " ++ pluralException (length brs) "branch" "branches" ++ " in parallel:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      (failures,nvrclogs) <- timeIO $ watchJobs Nothing [] [] jobs
      -- switch back to the original branch
      when (length brs /= 1) $
        gitSwitchBranch currentbranch
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      when (isNothing msidetagTarget) $ do
        let spec = packageSpec pkg
        bodhiUpdate dryrun mupdate Nothing False spec $
          intercalate "," . map jobNvr $
          filter ((/= Rawhide) . jobBranch) nvrclogs
      where
        -- FIXME time jobs
        setupBranch :: Branch -> IO JobAsync
        setupBranch br = do
          putPkgBrnchHdr pkg br
          target <- targetMaybeSidetag dryrun br msidetagTarget
          when (mmerge /= Just False) $ mergeNewerBranch Nothing br
          job <- startBuild Nothing 0 False False target pkg br "." >>= async
          unless dryrun $ sleep 3
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
      putStrLn $ "\n= Building parallel layer #" ++ show layernum ++
        if nopkgs > 1
        then " (" ++ show nopkgs +-+ "packages):"
        else ":"
      putStrLn $ unwords layer
      -- maybe print total pending packages
      unless (null nextLayers) $
        putStrLn $ plural layersleft "more layer" ++ " left with " ++
        let layerspkgs = map length nextLayers
        in case layerspkgs of
             [l] -> plural l "package"
             _ -> show layerspkgs +-+ "packages"
      jobs <- zipWithM setupBuild (reverse [0..(length layer - 1)]) layer
      when (null jobs) $
        error' "No jobs run"
      (failures,nvrs) <- watchJobs (Just layernum) [] [] jobs
      -- FIXME prompt to continue?
      if null failures
        then return nvrs
        else do
        let pending = sum $ map length nextLayers
        error' $ "Build failures in layer" +-+ show layernum ++ ": " ++
          unwords failures ++ "\n\n" ++
          plural pending "pending package" ++
          if pending > 0
          then
          ":\n" ++ unwords (map unwords nextLayers)
          else ""
      where
        nopkgs = length layer
        layersleft = length nextLayers

        setupBuild :: Int -> String -> IO JobAsync
        setupBuild n dir = do
          pkg <- getPackageName dir
          putPkgBrnchHdr pkg br
          job <- startBuild (Just layernum) n (layersleft > 0) (nopkgs > 5) target pkg br dir
                 >>= async
          unless dryrun $ sleep 4
          return (unPackage pkg,job)

    watchJobs :: Maybe Int -> [String] -> [JobDone] -> [JobAsync]
              -> IO ([String],[JobDone]) -- (failures,successes)
    watchJobs _ fails results [] = return (fails,results)
    watchJobs mlayer fails results (job:jobs) = do
      status <- poll (snd job)
      case status of
        Nothing -> sleep 1 >> watchJobs mlayer fails results (jobs ++ [job])
        -- (nvr,changelog)
        Just (Right result) -> do
          putStrLn $
            if null jobs
            then "ending layer" +-+ maybe "" show mlayer
            else plural (length jobs) "job" +-+ "left" +-+ maybe "" (\l ->  "in layer" +-+ show l) mlayer
          watchJobs mlayer fails (result:results) jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "** " ++ color Magenta pkg +-+ "job" +-+ color Magenta "failed" ++ " ** (" ++ plural (length jobs) "job" +-+ "left" +-+ maybe "" (\l ->  "in layer" +-+ show l) mlayer ++ ")"
          watchJobs mlayer (pkg : fails) results jobs

    -- FIXME prefix output with package name
    startBuild :: Maybe Int -> Int ->  Bool -> Bool -> String -> Package
               -> Branch -> String -> IO (IO JobDone)
    startBuild mlayer n morelayers background target pkg br dir =
      withExistingDirectory dir $ do
      gitSwitchBranch (RelBranch br)
      let spec = packageSpec pkg
      checkForSpecFile spec
      nvr <- pkgNameVerRel' br spec
      unpushed <- gitOneLineLog $ "origin/" ++ show br ++ "..HEAD"
      unless (null unpushed) $ do
        putStrLn $ nvr ++ " (" ++ target ++ ")" +-+ show n +-+ "more" +-+
          maybe "" (\l -> "in layer" +-+ show l) mlayer
        putNewLn
        displayCommits True unpushed
      unless (null unpushed) $ do
        checkSourcesMatch spec
        unless dryrun $
          gitPushSilent Nothing
      changelog <- unlines <$> getChangelog spec
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = if target == branchTarget br then branchDestTag br else target
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          -- FIXME detect old stable existing build
          putStrLn $ color Green nvr +-+ "is already" +-+ color Green "built"
          when (br /= Rawhide && morelayers && target == branchTarget br) $ do
            tags <- kojiNVRTags nvr
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
              unlessM (checkAutoBodhiUpdate br) $
              bodhiCreateOverride dryrun Nothing nvr
          return $ do
            when morelayers $
              kojiWaitRepo dryrun background target nvr
            return $ JobDone nvr br changelog
        Just BuildBuilding -> do
          putStrLn $ color Yellow nvr +-+ "is already" +-+ color Yellow "building"
          mtask <- kojiGetBuildTaskID fedoraHub nvr
          case mtask of
            Nothing -> error' $ "Task for " ++ nvr ++ " not found"
            Just task ->
              return $ do
              kojiWaitTaskAndRepo (isNothing mlatest) nvr task
              return $ JobDone nvr br changelog
        _ -> do
          when (null unpushed) $ do
            putStrLn $ nvr ++ " (" ++ target ++ ")" +-+ show n +-+ "more" +-+
              maybe "" (\l -> "in layer" +-+ show l) mlayer
            putNewLn
            putStrLn changelog
          buildref <- git "show-ref" ["--hash", "origin/" ++ show br]
          opentasks <- kojiOpenTasks pkg (Just buildref) target
          case opentasks of
            [task] -> do
              putStrLn $ nvr ++ " task is already open"
              return $ do
                kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                return $ JobDone nvr br changelog
            (_:_) -> error' $ show (length opentasks) ++ " open " ++ unPackage pkg ++ " tasks already"
            [] -> do
              if equivNVR nvr (fromMaybe "" mlatest)
                then return $ error' $
                     color Red $ nvr ++ " is already latest (modulo disttag)"
                else do
                -- FIXME parse build output
                if dryrun
                  then return $ return $ JobDone nvr br "<changelog>"
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing $ "--fail-fast" : ["--background" | background]
                  return $ do
                    kojiWaitTaskAndRepo (isNothing mlatest) nvr task
                    return $ JobDone nvr br changelog
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
            kojiWaitRepo dryrun background target nvr

    -- FIXME map nvr to package?
    renderChangelogs :: [JobDone] -> [String]
    renderChangelogs [] = []
    renderChangelogs ((JobDone nvr _ clog):jobs) =
      unlines [nvr, "", clog] : renderChangelogs jobs

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
            -- FIXME get updateid from above bodhi command output
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
                    putStr "Waiting for sidetag update to transition to 'request testing'"
                    sleep 80
                    bodhiUpdateTestingRequesting 1 updateid update
              _ -> error' $ "impossible happened: more than one update found for " ++ last nvrs

    bodhiUpdateTestingRequesting :: Double -> String -> Object -> IO ()
    bodhiUpdateTestingRequesting retries updateid update =
      if retries > 4
      then error' "\nupdate still not in 'request testing' status"
      else
        case lookupKey "request" update of
          Just (String request) ->
            putStrLn $ "\nrequest:" +-+ T.unpack request
          _ -> do
            mupdate' <- FedoraBodhi.bodhiUpdate updateid
            case mupdate' of
              Just update' -> do
                putChar '.'
                sleep (retries * 10)
                bodhiUpdateTestingRequesting (retries + 1) updateid update'
              _ -> error' "\nfailed to get updated metadata"
