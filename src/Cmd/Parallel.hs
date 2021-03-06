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

-- (pkg, (sidetag, nvr))
type Job = (String, Async (String, String))

-- FIXME only override if more packages to build
-- FIXME option to build multiple packages over branches in parallel
-- FIXME use --wait-build=NVR
-- FIXME check sources asap
parallelBuildCmd :: Bool -> Maybe SideTagTarget -> Maybe UpdateType -> Maybe BranchOpts -> [String]
                 -> IO ()
parallelBuildCmd dryrun msidetagTarget mupdatetype mbrnchopts args = do
  (brs,pkgs) <- splitBranchesPkgs True mbrnchopts True args
  when (null brs && isNothing mbrnchopts) $
    error' "Please specify at least one branch"
  branches <- listOfBranches True True mbrnchopts brs
  let mtarget = maybeTarget msidetagTarget
  when (isJust mtarget && length branches > 1) $
    error' "You can only specify target with one branch"
  if null pkgs
    then do
    unlessM isPkgGitRepo $
      error' "Please specify at least one package"
    parallelBranches $ map onlyRelBranch branches
    else do
    whenM isPkgGitRepo $
      error' "Cannot build multiple packages inside a package dir"
    forM_ branches $ \ br -> do
      case br of
        (RelBranch rbr) -> do
          layers <- dependencyLayers pkgs
          when (isNothing msidetagTarget && length layers > 1) $ do
            unlessM (checkAutoBodhiUpdate rbr) $
              error' "You must use --target/--sidetag to build package layers for this branch"
          when (length branches > 1) $
            putStrLn $ "# " ++ show rbr
          targets <- mapM (parallelBuild rbr) layers
          when (isJust msidetagTarget && null targets) $
            error' "No target was returned from jobs!"
          unless (isNothing msidetagTarget || null targets) $ do
            let target = head targets
            when (target /= branchTarget rbr) $ do
              notes <- prompt $ "Enter notes to submit Bodhi update for " ++ target
              bodhiSidetagUpdate target notes
        (OtherBranch _) ->
          error' "parallel builds only defined for release branches"
  where
    parallelBranches :: [Branch] -> IO ()
    parallelBranches brs = do
      krbTicket
      putStrLn $ "Building parallel " ++ show (length brs) ++ " branches:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      (failures,_mtarget) <- watchJobs Nothing [] jobs
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      where
        setupBranch :: Branch -> IO Job
        setupBranch br = do
          job <- startBuild False br "." >>= async
          sleep 5
          return (show br,job)

    parallelBuild :: Branch -> [String] -> IO String
    parallelBuild br layer =  do
      krbTicket
      when (nopkgs > 1) $ do
        putStrLn $ "\nBuilding parallel layer of " ++ show nopkgs ++ " packages:"
        putStrLn $ unwords layer
      jobs <- mapM setupBuild layer
      (failures,mtarget) <- watchJobs Nothing [] jobs
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      when (null jobs) $
        error' "No jobs run"
      return $ fromMaybe (error' "No target determined") mtarget
      where
        nopkgs = length layer

        setupBuild :: String -> IO Job
        setupBuild pkg = do
          job <- startBuild (nopkgs > 5) br pkg >>= async
          sleep 5
          return (pkg,job)

    watchJobs :: Maybe String -> [String] -> [Job] -> IO ([String],Maybe String)
    watchJobs mtarget fails [] = return (fails,mtarget)
    watchJobs mtarget fails (job:jobs) = do
      sleep 1
      status <- poll (snd job)
      case status of
        Nothing -> watchJobs mtarget fails (jobs ++ [job])
        Just (Right (target,nvr)) -> do
          putStrLn $ nvr ++ " job " ++ color Yellow "completed" ++  " (" ++ show (length jobs) ++ " jobs left)"
          watchJobs (Just target) fails jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "** " ++ pkg ++ " job " ++ color Magenta "failed" ++ " ** (" ++ show (length jobs) ++ " jobs left)"
          watchJobs mtarget (pkg : fails) jobs

    -- FIXME prefix output with package name
    startBuild :: Bool -> Branch -> String -> IO (IO (String,String))
    startBuild background br pkgdir =
      withExistingDirectory pkgdir $ do
      gitSwitchBranch (RelBranch br)
      pkg <- getPackageName pkgdir
      putPkgBrnchHdr pkg br
      unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
      unless (null unpushed) $
        mapM_ (putStrLn . simplifyCommitLog) unpushed
      let spec = packageSpec pkg
      checkForSpecFile spec
      unless (null unpushed) $ do
        checkSourcesMatch spec
        unless dryrun $
          gitPushSilent Nothing
      nvr <- pkgNameVerRel' br spec
      target <- case msidetagTarget of
                  Nothing -> return $ branchTarget br
                  Just (Target t) -> return t
                  Just SideTag -> do
                    tags <- map (head . words) <$> kojiUserSideTags br
                    case tags of
                      [] -> do
                        out <- head . lines <$> fedpkg "request-side-tag" []
                        if "Side tag '" `isPrefixOf` out then
                          return $ init . dropWhileEnd (/= '\'') $ dropPrefix "Side tag '" out
                          else error' "'fedpkg request-side-tag' failed"
                      [tag] -> return tag
                      _ -> error' $ "More than one user side-tag found for " ++ show br
      putStrLn $ nvr ++ " (" ++ target ++ ")\n"
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = if target == branchTarget br then branchDestTag br else target
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          putStrLn $ nvr ++ " is " ++ color Green "already built"
          when (br /= Rawhide && target == branchTarget br) $ do
            mtags <- kojiNVRTags nvr
            case mtags of
              Nothing -> error' $ nvr ++ " is untagged"
              Just tags ->
                unless (dryrun || any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
                  unlessM (checkAutoBodhiUpdate br) $
                  bodhiCreateOverride nvr
          return $ do
            unless dryrun $
              kojiWaitRepo target nvr
            return (target,nvr)
        Just BuildBuilding -> do
          putStrLn $ nvr ++ " is already building"
          return $
            kojiGetBuildTaskID fedoraHub nvr >>=
            maybe (error' $ "Task for " ++ nvr ++ " not found")
            (kojiWaitTaskAndRepo (isNothing mlatest) nvr target)
        _ -> do
          buildref <- git "show-ref" ["--hash", "origin" </> show br]
          opentasks <- kojiOpenTasks pkg (Just buildref) target
          case opentasks of
            [task] -> do
              putStrLn $ nvr ++ " task is already open"
              return $ kojiWaitTaskAndRepo (isNothing mlatest) nvr target task
            (_:_) -> error' $ show (length opentasks) ++ " open " ++ unPackage pkg ++ " tasks already"
            [] -> do
              if equivNVR nvr (fromMaybe "" mlatest)
                then return $ error' $ color Red $ nvr ++ " is already latest (modulo disttag)"
                else do
                -- FIXME parse build output
                if dryrun
                  then return (return (target,nvr))
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing $ "--fail-fast" : ["--background" | background]
                  return $ kojiWaitTaskAndRepo (isNothing mlatest) nvr target task
      where
        kojiWaitTaskAndRepo :: Bool -> String -> String -> TaskID -> IO (String,String)
        kojiWaitTaskAndRepo newpkg nvr target task = do
          finish <- kojiWatchTaskQuiet task
          if finish
            then putStrLn $ color Green $ nvr ++ " build success"
            else error' $ color Red $ nvr ++ " build failed"
          unless dryrun $ do
            autoupdate <- checkAutoBodhiUpdate br
            if autoupdate then
              when newpkg $ do
              mBugSess <- do
                (mbid, session) <- bzReviewSession
                return $ case mbid of
                  Just bid -> Just (bid,session)
                  Nothing -> Nothing
              whenJust mBugSess $
                \ (bid,session) -> putBugBuild session bid nvr
              else do
              when (target == branchTarget br) $
                -- -- FIXME: avoid prompt in
                -- changelog <- getChangeLog spec
                -- bodhiUpdate (fmap fst mBugSess) changelog nvr
                bodhiCreateOverride nvr
            kojiWaitRepo target nvr
          return (target,nvr)

    bodhiSidetagUpdate :: String -> String -> IO ()
    bodhiSidetagUpdate sidetag notes = do
      case mupdatetype of
        Nothing -> return ()
        Just updateType -> do
          putStrLn $ "Creating Bodhi Update for " ++ sidetag
          ok <- cmdBool "bodhi" ["updates", "new", "--type", show updateType , "--notes", "--request", "testing", if null notes then "to be written" else notes, "--autokarma", "--autotime", "--close-bugs", "--from-tag", sidetag]
          when ok $ do
            prompt_ "After editing update, press Enter to remove sidetag"
            fedpkg_ "remove-side-tag" [sidetag]
