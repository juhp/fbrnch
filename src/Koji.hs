module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiGetBuildTaskID,
  kojiLatestNVR,
  kojiOpenTasks,
  kojiScratchBuild,
  kojiUserSideTags,
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

kojiLatestNVRRepo :: String -> Int -> String -> IO (Maybe String)
kojiLatestNVRRepo tag event pkg = do
  mbld <- kojiLatestBuildRepo fedoraHub tag event pkg
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
  when srpm $ putStrLn "koji srpm build uploading..."
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
  checkResult <$> kojiBuildBranch' True target pkg mref args
  where
    checkResult = either (\ task -> error' (displayID task ++ " not completed")) (const ())

kojiBuildBranchNoWait ::String -> Package -> Maybe String -> [String] -> IO TaskID
kojiBuildBranchNoWait target pkg mref args = do
  Left task <- kojiBuildBranch' False target pkg mref args
  return task

kojiWaitRepo :: String -> String -> IO ()
kojiWaitRepo target nvr = do
  Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
  waitRepo buildtag Nothing
  where
    waitRepo :: String -> Maybe Struct -> IO ()
    waitRepo buildtag moldrepo = do
      when (isJust moldrepo) $ do
        threadDelay (50 * 1000 * 1000) -- 50s
        putChar '.'
      mrepo <- kojiGetRepo fedoraHub buildtag Nothing Nothing
      case mrepo of
        Nothing -> error' $ "failed to find koji repo for " ++ buildtag
        Just repo ->
          if moldrepo == mrepo
          then waitRepo buildtag mrepo
          else do
            let mevent = lookupStruct "create_event" repo
            case mevent of
              Nothing -> error "create_event not found"
              Just event -> do
                latest <- kojiLatestNVRRepo buildtag event (nameOfNVR nvr)
                if latest == Just nvr
                  then if isNothing moldrepo
                       then putStrLn $ nvr ++ " is in " ++ buildtag
                       else putStrLn "done"
                  else do
                  when (isNothing moldrepo) $ do
                    cmd_ "date" []
                    putStrLn $ "Waiting for " ++ buildtag ++ " to have " ++ nvr
                  waitRepo buildtag mrepo

kojiTagArchs :: String -> IO [String]
kojiTagArchs tag = do
  st <- Koji.getTag fedoraHub (Koji.InfoString tag) Nothing
  return $ maybe [] words $ lookupStruct "arches" st

kojiUserSideTags :: Branch -> IO [String]
kojiUserSideTags br = do
  mtags <- kojiBuildTarget fedoraHub (branchTarget br)
  case mtags of
    Nothing -> return []
    Just (buildtag,_desttag) -> do
      user <- fasIdFromKrb
      kojiListSideTags fedoraKojiHub (Just buildtag) (Just user)
