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

maybeTarget :: SideTagTarget -> Maybe String
maybeTarget (Target t) = Just t
maybeTarget _ = Nothing

-- (pkg, (sidetag, nvr))
type Job = (String, Async (String, String))

-- FIXME only override if more packages to build
-- FIXME option to build multiple packages over branches in parallel
-- FIXME use --wait-build=NVR
-- FIXME check sources asap
-- FIXME check not in pkg git dir
parallelBuildCmd :: Bool -> SideTagTarget -> Maybe BranchOpts -> [String]
                 -> IO ()
parallelBuildCmd dryrun sidetagTarget mbrnchopts args = do
  (brs,pkgs) <- splitBranchesPkgs True mbrnchopts args
  when (null brs && isNothing mbrnchopts) $
    error' "Please specify at least one branch"
  branches <- listOfBranches True True mbrnchopts brs
  let mtarget = maybeTarget sidetagTarget
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
          when (length branches > 1) $
            putStrLn $ "# " ++ show rbr
          layers <- dependencyLayers pkgs
          targets <- mapM (parallelBuild rbr) layers
          when (rbr /= Master && head targets == branchTarget rbr) $
            unless (null targets) $
            unlessM (checkAutoBodhiUpdate rbr) $
            return ()
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
          job <- startBuild br "." >>= async
          sleep 5
          return (show br,job)

    parallelBuild :: Branch -> [String] -> IO String
    parallelBuild br layer =  do
      krbTicket
      let nopkgs = length layer in
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
        setupBuild :: String -> IO Job
        setupBuild pkg = do
          job <- startBuild br pkg >>= async
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
    startBuild :: Branch -> String -> IO (IO (String,String))
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
      target <- case sidetagTarget of
                   Target t -> return t
                   SideTag -> do
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
          when (br /= Master && target == branchTarget br) $ do
            mtags <- kojiNVRTags nvr
            case mtags of
              Nothing -> error' $ nvr ++ " is untagged"
              Just tags ->
                unless (dryrun || any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
                  unlessM (checkAutoBodhiUpdate br) $
                  bodhiCreateOverride nvr
          return $ do
            unless dryrun $
              kojiWaitRepo True target nvr
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
                  task <- kojiBuildBranchNoWait target pkg Nothing ["--fail-fast", "--background"]
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
                \ (bid,session) -> postBuildComment session nvr bid
              else do
              when (target == branchTarget br) $
                -- -- FIXME: avoid prompt in
                -- changelog <- getChangeLog spec
                -- bodhiUpdate (fmap fst mBugSess) changelog nvr
                bodhiCreateOverride nvr
            kojiWaitRepo False target nvr
          return (target,nvr)
