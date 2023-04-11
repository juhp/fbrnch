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
  kojiWaitTask,
  TaskID,
  displayID,
  fedoraHub,
  maybeTimeout,
  targetMaybeSidetag
  ) where

import Data.Char (isDigit)

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Fixed (Micro)
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import SimplePrompt
import System.Exit
import System.Process.Typed
import System.Timeout (timeout)
import System.Time.Extra (sleep)

import Branches
import Common
import Common.System
import Git
import Krb
import Package (fedpkg, Package, unPackage)
import Pagure
import Types

fedoraHub :: String
fedoraHub = fedoraKojiHub

kojiNVRTags :: String -> IO [String]
kojiNVRTags nvr = do
  mbldid <- kojiGetBuildID fedoraHub nvr
  case mbldid of
    Nothing -> error' $ nvr +-+ "koji build not found"
    Just bldid -> kojiBuildTags fedoraHub (buildIDInfo bldid)

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

-- FIXME setTermTitle nvr
kojiBuild' :: Bool -> String -> [String] -> IO KojiBuildTask
kojiBuild' wait target args = do
  krbTicket
  let srpm = if null args
             then error' "no args passed to koji build"
             else ".src.rpm" `isSuffixOf` last args
  -- FIXME use tee functionality
  when srpm $ putStrLn "koji srpm build: uploading..."
  -- can fail like:
  -- [ERROR] koji: Request error: POST::https://koji.fedoraproject.org/kojihub/ssllogin::<PreparedRequest [POST]>
  -- [ERROR] koji: AuthError: unable to obtain a session
  -- readCreateProcess: koji "build" "--nowait" "f33-build-side-25385" "--fail-fast" "--background" ... (exit 1): failed
  (ret,out) <- readProcessStdout $ proc "koji" $ ["build", "--nowait", target] ++ args
  -- for srpm: drop uploading line until doing tee
  -- for git: drop "Created task: "
  -- init to drop final newline
  unless (B.null out) $
    logMsg $ (B.unpack . B.init . B.unlines . tail . B.lines) out
  if ret == ExitSuccess
    then do
    let kojiurl = B.unpack $ last $ B.words out
        task = (TaskId . read) $ takeWhileEnd isDigit kojiurl
    when wait $ do
      timeIO $ kojiWatchTask task
      cmd_ "date" ["+%T"]
    return $ if wait then Right kojiurl else Left task
    else do
    prompt_ "Press Enter to resubmit Koji build"
    kojiBuild' wait target args

-- kojiBuild :: String -> [String] -> IO String
-- kojiBuild target args = do
--   Right url <- kojiBuild' True target args
--   return url

-- FIXME filter/simplify output
-- FIXME implement native watchTask
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
    Just TaskFailed -> do
      let kojitool = "/usr/bin/koji-tool"
      whenM (doesFileExist kojitool) $
        cmd_ kojitool ["tasks", displayID task, "--tail", "-s", "fail"]
      error' "Task failed!"
    Just TaskCanceled -> return ()
    _ -> kojiWatchTask task

-- FIXME at 4am
-- Connection timed out: retrying
-- Connection timed out: retrying
-- Network.Socket.connect: <socket: 11>: does not exist (No route to host)
kojiWaitTask :: TaskID -> IO Bool
kojiWaitTask task = do
  -- FIXME can error:
  -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
  -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
  mst <- maybeTimeout 45 $ kojiGetTaskState fedoraHub task
  case mst of
    Just ts ->
      if ts `elem` openTaskStates
      then do
        -- this makes parallel too noisy
        --putChar '.'
        sleep 20
        kojiWaitTask task
      else return $ ts == TaskClosed
    Nothing -> do
      error $ "failed to get info for koji task" +-+ displayID task

kojiSource :: Package -> String -> String
kojiSource pkg ref =
  "git+https://" ++ srcfpo ++ "/rpms" +/+ unPackage pkg ++ ".git#" ++ ref

kojiBuildBranch' :: Bool -> String -> Package -> Maybe String -> [String]
                 -> IO KojiBuildTask
kojiBuildBranch' wait target pkg mref args = do
  commit <- maybe (git "rev-parse" ["HEAD"]) return mref
  kojiBuild' wait target $ args ++ [kojiSource pkg commit]

kojiBuildBranch :: String -> Package -> Maybe String -> [String] -> IO ()
kojiBuildBranch target pkg mref args =
  checkResult <$> kojiBuildBranch' True target pkg mref args
  where
    checkResult = either (\ task -> error' (displayID task +-+ "not completed")) (const ())

kojiBuildBranchNoWait ::String -> Package -> Maybe String -> [String] -> IO TaskID
kojiBuildBranchNoWait target pkg mref args = do
  Left task <- kojiBuildBranch' False target pkg mref args
  return task

kojiWaitRepo :: Bool -> Bool -> Bool -> String -> String -> IO ()
kojiWaitRepo dryrun quiet knowntag target nvr = do
  Just (buildtag,desttag) <- kojiBuildTarget fedoraHub target
  unless dryrun $ do
    mlatest <- kojiLatestNVR buildtag (nameOfNVR nvr)
    if Just nvr == mlatest
      then waitRepo buildtag Nothing
      else do
      tags <- cmdLines "koji" ["list-tags", "--build=" ++ nvr]
      if knowntag
        then do
        putStrLn $ "current tags:" +-+ unwords tags
        waitRepo buildtag Nothing
        else do
        mbuilt <- kojiLatestNVR desttag (nameOfNVR nvr)
        if mbuilt == Just nvr
          then do
          sleep 40
          kojiWaitRepo dryrun quiet knowntag target nvr
          else do
          putStrLn $ "current tags:" +-+ unwords tags
          unless (buildtag `elem` tags) $ do
            putStrLn $ "no" +-+ nvr +-+ "tagged" +-+ buildtag
            prompt_ "Press Enter to continue anyway"
          waitRepo buildtag Nothing
  where
    waitRepo :: String -> Maybe Struct -> IO ()
    waitRepo buildtag moldrepo = do
      when (isJust moldrepo) $ do
        threadDelay (fromEnum (50 :: Micro)) -- 50s
      mrepo <- kojiGetRepo fedoraHub buildtag Nothing Nothing
      case mrepo of
        Nothing -> error' $ "failed to find koji repo for" +-+ buildtag
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
                  then logMsg $ nvr +-+
                       if isNothing moldrepo
                       then "is in" +-+ buildtag
                       else "appeared"
                  else do
                  when (isNothing moldrepo && not quiet) $
                    logMsg $ "Waiting for" +-+ buildtag +-+ "to have" +-+ nvr
                  waitRepo buildtag mrepo

    -- FIXME: obsolete by using NVR
    -- n-v-r -> n
    nameOfNVR :: String -> String
    nameOfNVR = removeSeg . removeSeg
      where
        removeSeg = init . dropWhileEnd (/= '-')

kojiTagArchs :: String -> IO [String]
kojiTagArchs tag = do
  st <- Koji.getTag fedoraHub (Koji.InfoString tag) Nothing
  return $ maybe [] words $ lookupStruct "arches" st

kojiUserSideTags :: Maybe Branch -> IO [String]
kojiUserSideTags mbr = do
  user <- fasIdFromKrb
  case mbr of
    Nothing -> do
      maybeTimeout 55 $ kojiListSideTags fedoraKojiHub Nothing (Just user)
    Just br -> do
      mtags <- kojiBuildTarget fedoraHub (branchTarget br)
      case mtags of
        Nothing -> return []
        Just (buildtag,_desttag) -> do
          kojiListSideTags fedoraKojiHub (Just buildtag) (Just user)

maybeTimeout :: Micro -> IO a -> IO a
maybeTimeout secs act = do
  mres <- timeout (fromEnum secs) act
  case mres of
    Nothing -> do
      warning "Connection timed out: retrying"
      maybeTimeout (secs + 5) act
    Just res -> return res

targetMaybeSidetag :: Bool -> Branch -> Maybe SideTagTarget -> IO String
targetMaybeSidetag dryrun br msidetagTarget =
  case msidetagTarget of
    Nothing -> return $ branchTarget br
    Just (Target t) -> return t
    Just SideTag -> do
      tags <- map (head . words) <$> kojiUserSideTags (Just br)
      case tags of
        [] -> do
          Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub (show br)
          out <-
            if dryrun
            then return $ "Side tag '" ++ buildtag ++ "-dryrun'"
            else head . lines <$> fedpkg "request-side-tag" ["--base-tag",  buildtag]
          if "Side tag '" `isPrefixOf` out
            then do
            putStrLn out
            let sidetag =
                  init . dropWhileEnd (/= '\'') $ dropPrefix "Side tag '" out
            logMsg $ "Waiting for" +-+ sidetag +-+ "repo"
            unless dryrun $
              cmd_ "koji" ["wait-repo", sidetag]
            return sidetag
            else error' "'fedpkg request-side-tag' failed"
        [tag] -> return tag
        _ -> error' $ "More than one user side-tag found for" +-+ show br
