module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiGetBuildState,
  kojiGetBuildTaskID,
  kojiLatestNVR,
  kojiOpenTasks,
  kojiScratchBuild,
  kojiUserSideTags,
  buildIDInfo,
  BuildInfo(BuildInfoNVR),
  BuildState(..),
  kojiBuildBranch,
  kojiBuildBranchNoWait,
  kojiSource,
  kojiBuildTarget',
  kojiTagArchs,
  kojiWaitRepoNVR,
  kojiWaitRepoNVRs,
  kojiWaitRepo,
  kojiWatchTask,
  kojiWaitTask,
  putTaskinfoUrl,
  TaskID,
  displayID,
  fedoraHub,
  getKojiProfileHub,
  maybeTimeout,
  createKojiSidetag,
  targetMaybeSidetag
  ) where

import Data.Char (isDigit)

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Fixed (Micro)
import Data.RPM.NVR (NVR, maybeNVR)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Distribution.Koji
import qualified Distribution.Koji.API as Koji
import Distribution.Fedora.Branch (branchRelease)
import Distribution.Fedora.Release (releaseDistTag)
import Fedora.Krb (fasIdFromKrb, krbTicket)
import Safe (headMay, tailSafe)
import Say (sayString)
import SimplePrompt (promptEnter, promptNonEmpty, yesNo)
import System.Environment (lookupEnv)
import System.Exit
import System.Process.Typed
import System.Timeout (timeout)
import System.Time.Extra (sleep)

import Branches
import Common
import Common.System
import Git
import Package (fedpkg)
import Pagure
import Types

fedoraHub :: String
fedoraHub = fedoraKojiHub

getKojiProfile :: IO (Maybe String)
getKojiProfile = lookupEnv "FBRNCH_KOJI_PROFILE"

getKojiProfileHub' :: Maybe String -> IO String
getKojiProfileHub' mprofile = do
  case mprofile of
    Nothing -> return fedoraKojiHub
    Just p -> do
      let file = "/etc/koji.conf.d" </> p <.> "conf"
      exists <- doesFileExist file
      if exists
        then do
        serv <- grep "^server =" $ "/etc/koji.conf.d" </> p <.> "conf"
        case serv of
          [sl] ->
            case words sl of
              ["server","=",hub] -> return hub
              _ -> error' $ "Invalid server config in" +-+ file
          _ -> error' $ "Undefined server in" +-+ file
        else error' $ "Undefined koji profile:" +-+ p

getKojiProfileHub :: IO String
getKojiProfileHub =
  getKojiProfile >>= getKojiProfileHub'

kojiNVRTags :: NVR -> IO [String]
kojiNVRTags nvr = do
  hub <- getKojiProfileHub
  mbldid <- kojiGetBuildID hub $ showNVR nvr
  case mbldid of
    Nothing -> error' $ showNVR nvr +-+ "koji build not found"
    Just bldid -> kojiBuildTags hub (buildIDInfo bldid)

kojiBuildStatus :: NVR -> IO (Maybe BuildState)
kojiBuildStatus nvr = do
  hub <- getKojiProfileHub
  kojiGetBuildState hub (BuildInfoNVR (showNVR nvr))

kojiLatestNVR :: String -> String -> IO (Maybe NVR)
kojiLatestNVR tag pkg = do
  hub <- getKojiProfileHub
  mbld <- kojiLatestBuild hub tag pkg
  return $ case mbld of
             Nothing -> Nothing
             Just bld -> lookupStruct "nvr" bld >>= maybeNVR


kojiOpenTasks :: Package -> Maybe String -> String -> IO [TaskID]
kojiOpenTasks pkg mref target = do
  hub <- getKojiProfileHub
  user <- fasIdFromKrb
  muserid <- kojiGetUserID hub user
  let userid = fromMaybe (error' $ "Koji failed to return userid for '" ++ user ++ "'") muserid
  commit <- maybe (git "rev-parse" ["HEAD"]) return mref
  let source = kojiSource pkg commit
  kojiUserBuildTasks hub userid (Just source) (Just target)

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
  mprofile <- getKojiProfile
  -- can fail like:
  -- [ERROR] koji: Request error: POST::https://koji.fedoraproject.org/kojihub/ssllogin::<PreparedRequest [POST]>
  -- [ERROR] koji: AuthError: unable to obtain a session
  -- readCreateProcess: koji "build" "--nowait" "f33-build-side-25385" "--fail-fast" "--background" ... (exit 1): failed
  (ret,out) <- readProcessStdout $ proc "koji" $ kojiProfileOpt mprofile ++ ["build", "--nowait", target] ++ args
  -- for srpm: drop uploading line until doing tee
  -- for git: drop "Created task: "
  -- init to drop final newline
  unless (B.null out) $
    -- FIXME include output example here
    logMsg $ (dropPrefix "Task info: " . B.unpack . B.init . B.unlines . tailSafe . B.lines) out
  if ret == ExitSuccess
    then do
    let kojiurl = B.unpack $ last $ B.words out
        task = (TaskId . read) $ takeWhileEnd isDigit kojiurl
    when wait $ do
      -- FIXME get actual build time
      timeIO $ kojiWatchTask task
      cmd_ "date" ["+%T"]
    return $ if wait then Right kojiurl else Left task
    else do
    promptEnter "Press Enter to resubmit Koji build"
    kojiBuild' wait target args

-- kojiBuild :: String -> [String] -> IO String
-- kojiBuild target args = do
--   Right url <- kojiBuild' True target args
--   return url

kojiProfileOpt :: Maybe String -> [String]
kojiProfileOpt Nothing = []
kojiProfileOpt (Just p) = ["--profile", p]

-- FIXME filter/simplify output
-- FIXME implement native watchTask
kojiWatchTask :: TaskID -> IO ()
kojiWatchTask task = do
  mprofile <- getKojiProfile
  -- FIXME can error:
  -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
  -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
  -- eg3 [ERROR] koji: ReadTimeout: HTTPSConnectionPool(host='koji.fedoraproject.org', port=443): Read timed out. (read timeout=43200)
  -- This might error with exit 0 occasionally so we check the taskstate always
  void $ cmdBool "koji" $ kojiProfileOpt mprofile ++ ["watch-task", displayID task]
  hub <- getKojiProfileHub' mprofile
  mst <- kojiGetTaskState hub task
  case mst of
    Just TaskClosed -> return ()
    Just TaskFailed -> do
      whenJustM (findExecutable "koji-tool") $ \kojitool -> do
        cmdLog_ kojitool ["tasks", "--hub", hub, "--children", displayID task, "--tail", "-s", "fail"]
        putTaskinfoUrl hub task
      error' "Task failed!"
    Just TaskCanceled -> return ()
    _ -> kojiWatchTask task

putTaskinfoUrl :: String -> TaskID -> IO ()
putTaskinfoUrl hub tid =
  putStrLn $ dropSuffix "hub" hub +/+ "taskinfo?taskID=" ++ show (getID tid)

-- FIXME during network disconnection:
-- Connection timed out: retrying
-- Connection timed out: retrying
-- Network.Socket.connect: <socket: 11>: does not exist (No route to host)
kojiWaitTask :: TaskID -> IO Bool
kojiWaitTask task = do
  hub <- getKojiProfileHub
  -- FIXME can error:
  -- eg1 [ERROR] koji: HTTPError: 503 Server Error: Service Unavailable for url: https://koji.fedoraproject.org/kojihub
  -- eg2 [ERROR] koji: ServerOffline: database outage: - user error (Error 1014: database outage)
  mst <- maybeTimeout 45 $ kojiGetTaskState hub task
  case mst of
    Just ts ->
      if ts `elem` openTaskStates
      then do
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

-- remove once in koji-hs
kojiBuildTarget' :: String -- ^ hubUrl
                 -> String -- ^ target
                 -> IO (String, String) -- ^ (build-tag,dest-tag)
kojiBuildTarget' hub target = do
  mres <- kojiBuildTarget hub target
  case mres of
    Nothing -> error' $ "failed to get BuildTarget for" +-+ target
    Just res -> return res

-- FIXME should be NonEmpty
-- FIXME add back knowntag?
kojiWaitRepoNVRs :: Bool -> Bool -> String -> [NVR] -> IO ()
kojiWaitRepoNVRs _ _ _ [] = error' "no NVRs given to wait for"
kojiWaitRepoNVRs dryrun quiet target nvrs = do
  hub <- getKojiProfileHub
  (buildtag,_desttag) <- kojiBuildTarget' hub target
  unless dryrun $ do
    tz <- getCurrentTimeZone
    unless quiet $
      logSay tz $ "Waiting for" +-+ buildtag +-+ "to have" +-+
      case nvrs of
        [nvr] -> showNVR nvr
        _ -> "builds"
    mprofile <- getKojiProfile
    -- FIXME use knowntag to quieten output: for override outputs, eg
    -- "nvr ghc-rpm-macros-2.7.5-1.fc41 is not current in tag f41-build
    --    latest build is ghc-rpm-macros-2.7.2-4.fc41"
    -- or "No clang18 builds in tag epel10.1-build"
    void $ timeIO $ cmd "koji" (kojiProfileOpt mprofile ++ ["wait-repo", "--request", "--quiet"] ++ ["--build=" ++ showNVR nvr | nvr <- nvrs] ++ [buildtag])

kojiWaitRepoNVR :: Bool -> Bool -> String -> NVR -> IO ()
kojiWaitRepoNVR dryrun quiet target nvr =
  kojiWaitRepoNVRs dryrun quiet target [nvr]

-- FIXME display more status/age info
kojiWaitRepo :: Bool -> Bool -> String -> IO ()
kojiWaitRepo dryrun quiet target = do
  hub <- getKojiProfileHub
  (buildtag,_desttag) <- kojiBuildTarget' hub target
  tz <- getCurrentTimeZone
  unless quiet $
    logSay tz $ "Waiting for" +-+ buildtag
  unless dryrun $ do
    mprofile <- getKojiProfile
    void $ timeIO $ cmd "koji" $ kojiProfileOpt mprofile ++ ["wait-repo", "--request", "--quiet", buildtag]

kojiTagArchs :: String -> IO [String]
kojiTagArchs tag = do
  hub <- getKojiProfileHub
  st <- Koji.getTag hub (Koji.InfoString tag) Nothing
  return $ maybe [] words $ lookupStruct "arches" st

kojiUserSideTags :: Maybe Branch -> IO [String]
kojiUserSideTags mbr = do
  hub <- getKojiProfileHub
  user <- fasIdFromKrb
  mapMaybe (headMay . words) <$>
    case mbr of
      Nothing -> do
        maybeTimeout 55 $ kojiListSideTags hub Nothing (Just user)
      Just br -> do
        mtags <- kojiBuildTarget hub (showBranch br)
        case mtags of
          Nothing -> return []
          Just (buildtag,_desttag) ->
            kojiListSideTags hub (Just buildtag) (Just user)

maybeTimeout :: Micro -> IO a -> IO a
maybeTimeout secs act = do
  mres <- timeout (fromEnum secs) act
  case mres of
    Nothing -> do
      warning "Connection timed out: retrying"
      maybeTimeout (secs + 5) act
    Just res -> return res

createKojiSidetag :: Bool -> Branch -> IO String
createKojiSidetag dryrun br = do
  hub <- getKojiProfileHub
  (buildtag,_desttag) <- kojiBuildTarget' hub (showBranch br)
  out <-
    if dryrun
    then return $ "Side tag '" ++ buildtag ++ "'"
    else do
      krbTicket
      ls <- lines <$> fedpkg "request-side-tag" ["--base-tag",  buildtag]
      case ls of
        [] -> error' "no output from request-side-tag"
        (l:_) -> return l
  if "Side tag '" `isPrefixOf` out
    then do
    putNewLn
    let sidetag =
          init . dropWhileEnd (/= '\'') $ dropPrefix "Side tag '" out
    putStrLn $ "Sidetag" +-+ sidetag +-+ "created"
    -- logMsg $ "Waiting for" +-+ sidetag +-+ "repo"
    -- unless dryrun $
    --   cmd_ koji ["wait-repo", sidetag]
    return sidetag
    else error' "'fedpkg request-side-tag' failed"

targetMaybeSidetag :: Bool -> Bool -> Bool -> Branch -> Maybe SideTagTarget
                   -> IO String
targetMaybeSidetag dryrun strict create br msidetagTarget =
  case msidetagTarget of
    Nothing -> return $ showBranch br
    Just (Target t) -> do
      disttag <- releaseDistTag <$> branchRelease br
      if t == "rawhide" && br == Rawhide
        then return disttag
        else do
        unless (disttag `isPrefixOf` t) $ do
          let msg = "Branch" +-+ showBranch br +-+ "does not match target" +-+ t in
            if strict
            then do
              ok <- yesNo $ msg ++ "! Are you sure?"
              unless ok $ error' "aborted"
            else
              whenM isPkgGitRepo $
              warning ("Warning:" +-+ msg)
        return t
    Just SideTag -> do
      tags <- kojiUserSideTags (Just br)
      case tags of
        [] ->
          if create
          then createKojiSidetag dryrun br
          else error' "no existing side-tag"
        [tag] -> return tag
        _ -> do
          putStrLn $ "User side-tags found for" +-+ showBranch br ++ ":"

          let num = length tags
              nums = map show [1..num]
              numtags = zip nums $ sort tags
          mapM_ (putStrLn . \(n,s) -> n ++ ":" +-+ s) numtags
          tag <- promptTags numtags $ "Choose" +-+ (if num == 2 then "1 or 2" else "1-" ++ show num) +-+ "or enter sidetag; or 0 to abort"
          putStrLn $ "will use" +-+ tag
          putNewLn
          return tag

logSay :: TimeZone -> String -> IO ()
logSay tz str = do
  now <- utcToLocalTime tz <$> getCurrentTime
  sayString $ formatTime defaultTimeLocale "%T" now +-+ str

promptTags :: [(String,String)] -> String -> IO String
promptTags numtags txt = do
  ans <- promptNonEmpty txt
  if ans `elem` map snd numtags
    then return ans
    else
    case lookup ans numtags of
      Just t -> return t
      Nothing ->
        if lower ans `elem` ["0", "q", "quit", "no"]
        then error' "aborting"
        else promptTags numtags txt
