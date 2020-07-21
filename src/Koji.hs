module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiGetBuildTaskID,
  kojiLatestNVR,
  kojiOpenTasks,
  kojiScratchBuild,
  kojiScratchUrl,
  buildIDInfo,
  BuildState(..),
  kojiBuild,
  kojiBuildBranch,
  kojiBuildBranchNoWait,
  kojiSource,
  kojiWaitRepo,
  kojiWatchTask,
  kojiWatchTaskQuiet,
  TaskID,
  fedoraHub
  ) where

import Data.Char (isDigit)

import Fedora.Koji

import Branches
import Common
import Common.System
import Git
import Krb
import Package

fedoraHub :: String
fedoraHub = fedoraKojiHub

kojiNVRTags :: String -> IO (Maybe [String])
kojiNVRTags nvr = do
  mbldid <- kojiGetBuildID fedoraHub nvr
  case mbldid of
    Nothing -> return Nothing
    Just bldid -> Just <$> kojiBuildTags fedoraHub (buildIDInfo bldid)

kojiBuildStatus :: String -> IO (Maybe BuildState)
kojiBuildStatus nvr =
  kojiGetBuildState fedoraHub (BuildInfoNVR nvr)

kojiLatestNVR :: String -> String -> IO (Maybe String)
kojiLatestNVR tag pkg = do
  mbld <- kojiLatestBuild fedoraHub tag pkg
  return $ case mbld of
             Nothing -> Nothing
             Just bld -> lookupStruct "nvr" bld

kojiOpenTasks :: Package -> Maybe String -> String -> IO [TaskID]
kojiOpenTasks pkg mref target = do
  user <- fasIdFromKrb
  muserid <- kojiGetUserID fedoraHub user
  let userid = fromMaybe (error' $ "Koji failed to return userid for '" ++ user ++ "'") muserid
  commit <- maybe (git "rev-parse" ["HEAD"]) return mref
  let source = kojiSource pkg commit
  kojiUserBuildTasks fedoraHub userid (Just source) (Just target)

-- * Koji building

kojiScratchUrl :: Bool -> String -> IO (Maybe String)
kojiScratchUrl noscratch srpm =
    if noscratch
    then return Nothing
    else Just <$> kojiScratchBuild "rawhide" [] srpm

kojiScratchBuild :: String -> [String] -> FilePath -> IO String
kojiScratchBuild target args srpm = do
  Right url <- kojiBuild' True target $ args ++ ["--scratch", "--no-rebuild-srpm", srpm]
  return url

type KojiBuildTask = Either TaskID String

-- FIXME can fail like:
-- [ERROR] koji: AuthError: unable to obtain a session
-- readCreateProcess: koji "build" "--nowait" "f33-build-side-25385" "--fail-fast" "--background" ... (exit 1): failed
kojiBuild' :: Bool -> String -> [String] -> IO KojiBuildTask
kojiBuild' wait target args = do
  krbTicket
  cmd_ "date" []
  -- FIXME setTermTitle nvr
  out <- cmd "koji" $ ["build", "--nowait", target] ++ args
  putStrLn out
  let kojiurl = last $ words out
      task = (TaskId . read) $ takeWhileEnd isDigit kojiurl
  when wait $ kojiWatchTask task
  return $ if wait then Right kojiurl else Left task

kojiBuild :: String -> [String] -> IO String
kojiBuild target args = do
  Right url <- kojiBuild' True target args
  return url

-- FIXME filter/simplify output
kojiWatchTask :: TaskID -> IO ()
kojiWatchTask task =
  ifM (cmdBool "koji" ["watch-task", displayID task])
  (return ()) $
  do
    -- FIXME can error:
    -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
    -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
    mst <- kojiGetTaskState fedoraHub task
    case mst of
      Just TaskClosed -> return ()
      Just TaskFailed -> error "Task failed!"
      _ -> kojiWatchTask task

kojiWatchTaskQuiet ::TaskID -> IO Bool
kojiWatchTaskQuiet task =
  ifM (cmdBool "koji" ["watch-task", "--quiet", displayID task])
  (return True) $
  do
    -- FIXME can error:
    -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
    -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
    mst <- kojiGetTaskState fedoraHub task
    case mst of
      Just TaskClosed -> return True
      Just TaskFailed -> return False
      _ -> kojiWatchTaskQuiet task

kojiSource :: Package -> String -> String
kojiSource pkg ref =
  "git+https://src.fedoraproject.org/rpms" </> unPackage pkg ++ ".git#" ++ ref

kojiBuildBranch' :: Bool -> String -> Package -> Maybe String -> [String]
                 -> IO KojiBuildTask
kojiBuildBranch' wait target pkg mref args = do
  commit <- maybe (git "rev-parse" ["HEAD"]) return mref
  kojiBuild' wait target $ args ++ [kojiSource pkg commit]

kojiBuildBranch :: String -> Package -> Maybe String -> [String] -> IO ()
kojiBuildBranch target pkg mref args =
  void $ kojiBuildBranch' True target pkg mref args

kojiBuildBranchNoWait ::String -> Package -> Maybe String -> [String] -> IO TaskID
kojiBuildBranchNoWait target pkg mref args = do
  Left task <- kojiBuildBranch' False target pkg mref args
  return task

-- FIXME use koji-hs
kojiWaitRepo :: Branch -> String -> String -> IO ()
kojiWaitRepo br target nvr =
  cmd_ "koji" ["wait-repo", targetTag, "--build=" ++ nvr]
  where
    targetTag =
      if target == branchTarget br
      then target ++ "-build"
      else target
