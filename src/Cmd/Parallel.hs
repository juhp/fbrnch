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

data SideTagTarget = SideTag | Target String

maybeTarget :: Maybe SideTagTarget -> Maybe String
maybeTarget (Just (Target t)) = Just t
maybeTarget _ = Nothing

type Job = (String, Async String)

-- FIXME option to build multiple packages over branches in parallel
-- FIXME require --with-side-tag or --target
-- FIXME use --wait-build=NVR
-- FIXME check sources asap
-- FIXME check not in pkg git dir
parallelBuildCmd :: Bool -> Maybe SideTagTarget -> Maybe BranchOpts -> [String]
                 -> IO ()
parallelBuildCmd dryrun msidetagTarget mbrnchopts args = do
  (brs,pkgs) <- splitBranchesPkgs True mbrnchopts args
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
    else
    forM_ branches $ \ br -> do
      case br of
        (RelBranch rbr) -> do
          putStrLn $ "# " ++ show rbr
          layers <- dependencyLayers pkgs
          mapM_ (parallelBuild rbr) layers
        (OtherBranch _) ->
          error' "parallel builds only defined for release branches"
  where
    parallelBranches :: [Branch] -> IO ()
    parallelBranches brs = do
      krbTicket
      putStrLn $ "Building parallel " ++ show (length brs) ++ " branches:"
      putStrLn $ unwords $ map show brs
      jobs <- mapM setupBranch brs
      failures <- watchJobs [] jobs
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      where
        setupBranch :: Branch -> IO Job
        setupBranch br = do
          job <- startBuild br "." >>= async
          sleep 5
          return (show br,job)

    parallelBuild :: Branch -> [String] -> IO ()
    parallelBuild br layer =  do
      krbTicket
      putStrLn $ "\nBuilding parallel layer of " ++ show (length layer) ++ " packages:"
      putStrLn $ unwords layer
      jobs <- mapM setupBuild layer
      failures <- watchJobs [] jobs
      unless (null failures) $
        error' $ "Build failures: " ++ unwords failures
      where
        setupBuild :: String -> IO Job
        setupBuild pkg = do
          job <- startBuild br pkg >>= async
          sleep 5
          return (pkg,job)

    watchJobs :: [String] -> [Job] -> IO [String]
    watchJobs fails [] = return fails
    watchJobs fails (job:jobs) = do
      sleep 1
      status <- poll (snd job)
      case status of
        Nothing -> watchJobs fails (jobs ++ [job])
        Just (Right nvr) -> do
          putStrLn $ nvr ++ " job " ++ color Yellow "completed" ++  " (" ++ show (length jobs) ++ " jobs left)"
          watchJobs fails jobs
        Just (Left except) -> do
          print except
          let pkg = fst job
          putStrLn $ "** " ++ pkg ++ " job " ++ color Magenta "failed" ++ " ** (" ++ show (length jobs) ++ " jobs left)"
          watchJobs (pkg : fails) jobs

    -- FIXME prefix output with package name
    startBuild :: Branch -> String -> IO (IO String)
    startBuild br pkgdir =
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
      putStrLn $ nvr ++ "\n"
      mtarget <- case msidetagTarget of
                   Nothing -> return Nothing
                   Just (Target t) -> return $ Just t
                   Just SideTag -> do
                     tags <- map (head . words) . lines <$> fedpkg "list-side-tags" ["--mine"]
                     let tgt = branchTarget br
                     case filter ((tgt ++ "-") `isPrefixOf`) tags of
                       [] -> error' $ "No user side-tag found: please create with 'fedpkg request-side-tag --base-tag " ++ tgt
                       [tag] -> return $ Just tag
                       _ -> error' $ "More than one user side-tag found for " ++ tgt
      let target = fromMaybe (branchTarget br) mtarget
      -- FIXME should compare git refs
      -- FIXME check for target
      buildstatus <- kojiBuildStatus nvr
      let tag = fromMaybe (branchDestTag br) mtarget
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      case buildstatus of
        Just BuildComplete -> do
          putStrLn $ nvr ++ " is already built"
          when (br /= Master && isNothing msidetagTarget) $ do
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
            return nvr
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
                  then return (return nvr)
                  else do
                  task <- kojiBuildBranchNoWait target pkg Nothing ["--fail-fast", "--background"]
                  return $ kojiWaitTaskAndRepo (isNothing mlatest) nvr target task
      where
        kojiWaitTaskAndRepo :: Bool -> String -> String -> TaskID -> IO String
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
                \ (bid,session) -> postBuildComment session nvr bid
              else do
              when (isNothing msidetagTarget) $
                -- -- FIXME: avoid prompt in
                -- changelog <- getChangeLog spec
                -- bodhiUpdate (fmap fst mBugSess) changelog nvr
                bodhiCreateOverride nvr
            kojiWaitRepo target nvr
          return nvr
