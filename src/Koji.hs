module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiGetBuildTaskID,
  kojiLatestNVR,
  kojiOpenTasks,
  kojiScratchBuild,
  buildIDInfo,
  BuildState(..),
  kojiBuildBranch,
  kojiBuildBranchNoWait,
  kojiSource,
  kojiBuildTarget,
  kojiTagArchs,
  kojiWaitRepo,
  kojiWatchTask,
  kojiWatchTaskQuiet,
  TaskID,
  displayID,
  fedoraHub
  ) where

import Data.Char (isDigit)

import Control.Concurrent (threadDelay)
import Fedora.Koji
import qualified Fedora.Koji.Internal as Koji

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
  let srpm = if null args
             then error' "no args passed to koji build"
             else ".src.rpm" `isSuffixOf` last args
  -- FIXME use tee functionality
  when srpm $ putStrLn "uploading srpm..."
  -- FIXME setTermTitle nvr
  out <- cmd "koji" $ ["build", "--nowait", target] ++ args
  putStrLn $ if srpm
    -- drop uploading line until doing tee
    then (unlines . tail . lines) out
    else out
  let kojiurl = last $ words out
      task = (TaskId . read) $ takeWhileEnd isDigit kojiurl
  when wait $ do
    kojiWatchTask task
    cmd_ "date" []
  return $ if wait then Right kojiurl else Left task

-- kojiBuild :: String -> [String] -> IO String
-- kojiBuild target args = do
--   Right url <- kojiBuild' True target args
--   return url

-- FIXME filter/simplify output
kojiWatchTask :: TaskID -> IO ()
kojiWatchTask task = do
  -- FIXME can error:
  -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
  -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
  -- eg3 [ERROR] koji: ReadTimeout: HTTPSConnectionPool(host='koji.fedoraproject.org', port=443): Read timed out. (read timeout=43200)
  -- This might error with exit 0 occasionally so we check the taskstate always
  void $ cmdBool "koji" ["watch-task", displayID task]
  mst <- kojiGetTaskState fedoraHub task
  case mst of
    Just TaskClosed -> return ()
    Just TaskFailed -> error "Task failed!"
    Just TaskCanceled -> return ()
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
kojiWaitRepo :: Bool -> String -> String -> IO ()
kojiWaitRepo built target nvr = do
  Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
  cmd_ "date" []
  putStrLn $ "Running wait-repo for " ++ buildtag ++ " (" ++ nvr ++ ")"
  unless built $ waitRepo buildtag []
  waitRepo buildtag ["--build=" ++ nvr]
  where
    waitRepo :: String -> [String] -> IO ()
    waitRepo buildtag args =
      cmdFragile_ "koji" $ ["wait-repo", buildtag] ++ args

cmdFragile_ :: String -> [String] -> IO ()
cmdFragile_ c as = do
  ok <- cmdBool c as
  if ok then return ()
    else do
    warning $ "retrying \"" ++ c +-+ unwords as ++ "\""
    threadDelay 2000000
    cmdFragile_ c as

kojiTagArchs :: String -> IO [String]
kojiTagArchs tag = do
  st <- Koji.getTag fedoraHub (Koji.InfoString tag) Nothing
  return $ maybe [] words $ lookupStruct "arches" st
