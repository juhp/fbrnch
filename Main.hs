{-# LANGUAGE CPP #-}

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad
import Data.Char (isAscii, toLower)
import Data.Ini.Config
import Data.List
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Directory (Manager, httpExists, httpManager)
import Network.HTTP.Simple
import Network.URI (isURI)
import Options.Applicative (maybeReader)
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)
import System.Process.Text (readProcessWithExitCode)

import Web.Bugzilla
import Web.Bugzilla.Search

import NewId
import ValidLogin

type Package = String

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  activeBranches <- getFedoraBranches
  dispatchCmd activeBranches

dispatchCmd :: [Branch] -> IO ()
dispatchCmd activeBranches =
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "create-review" "Create a Package Review request" $
      createReview <$> optional (strArg "SPECFILE")
    , Subcommand "update-review" "Update a Package Review" $
      updateReview <$> switchWith 'n' "no-scratch" "Add a Koji scratch build" <*> optional (strArg "SPECFILE")
    , Subcommand "approved" "List approved reviews" $
      pure approvedCmd
    , Subcommand "request" "Request dist git repo for new package" $
      requestRepo <$> strArg "NEWPACKAGE"
    , Subcommand "import" "Import new package via bugzilla" $
      importPkgs <$> many (strArg "NEWPACKAGE...")
    , Subcommand "build" "Build package(s)" $
      build <$> mockOpt <*> branchOpt <*> some pkgArg
    , Subcommand "build-branch" "Build branch(s) of package" $
      buildBranch False Nothing <$> pkgOpt <*> mockOpt <*> some branchArg
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> some (strArg "PACKAGE...")
    , Subcommand "list" "List package reviews" $
      pure listReviews
    , Subcommand "review" "Find package review bug" $
      review <$> strArg "PACKAGE"
    ]
  where
    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH.."

    branchOpt :: Parser (Maybe Branch)
    branchOpt = optional (optionWith branchM 'b' "branch" "BRANCH" "branch")

    branchM = maybeReader (readBranch activeBranches)

    pkgArg :: Parser Package
    pkgArg = removeSuffix "/" <$> strArg "PACKAGE.."

    pkgOpt :: Parser (Maybe String)
    pkgOpt = optional (strOptionWith 'p' "package" "PKG" "package")

    mockOpt = switchWith 'm' "mock" "Do mock build to test branch"

fedpkg :: String -> [String] -> IO String
fedpkg c args =
  cmd "fedpkg" (c:args)

fedpkg_ :: String -> [String] -> IO ()
fedpkg_ c args =
  cmd_ "fedpkg" (c:args)

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,2))
#else
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

getPackageBranches :: IO [Branch]
getPackageBranches = do
  activeBranches <- getFedoraBranches
  mapMaybe (readBranch' activeBranches . removePrefix "origin/") . words <$> cmd "git" ["branch", "--remote", "--list"]

withExistingDirectory :: FilePath -> IO () -> IO ()
withExistingDirectory dir act = do
  hasDir <- doesDirectoryExist dir
  if not hasDir
    then error' $ "No such directory: " ++ dir
    else
    withCurrentDirectory dir act

build :: Bool -> Maybe Branch -> [Package] -> IO ()
build _ _ [] = return ()
build mock mbr (pkg:pkgs) = do
  fedBranches <- getFedoraBranches
  withExistingDirectory pkg $ do
    gitPull
    branches <- case mbr of
      Just b | b `elem` fedBranches -> return [b]
             | otherwise -> error' "Unsupported branch"
      Nothing ->
        -- FIXME problem is we may want --all: maybe better just to request --all
        filter (`elem` fedBranches) <$> getPackageBranches
    buildBranch True Nothing (Just pkg) mock branches
  build mock mbr pkgs

gitPull :: IO ()
gitPull = do
  pull <- git "pull" ["--rebase"]
  unless ("Already up to date." `isPrefixOf` pull) $
    putStrLn pull

putPkgBrnchHdr :: String -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ pkg ++ ":" ++ show br ++ " =="

buildBranch :: Bool -> Maybe Branch -> Maybe Package -> Bool -> [Branch] -> IO ()
buildBranch _ _ _ _ [] = return ()
buildBranch pulled mprev mpkg mock (br:brs) = do
  checkWorkingDirClean
  unless pulled gitPull
  pkg <- maybe getPackageDir return mpkg
  putPkgBrnchHdr pkg br
  branched <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
  if not branched then
    if br == Master
    then error' "no origin/master found!"
    else do
      checkNoBranchRequest pkg
      when mock $ fedpkg_ "mockbuild" ["--root", mockConfig br]
      putStrLn $ "requesting branch " ++ show br
      -- FIXME? request all branches?
      url <- fedpkg "request-branch" [show br]
      putStrLn url
      postBranchReq url
    else do
    current <- git "rev-parse" ["--abbrev-ref", "HEAD"]
    when (current /= show br) $
      fedpkg_ "switch-branch" ["--fetch", show br]
    prev <- case mprev of
              Just p -> return p
              Nothing -> do
                branches <- getFedoraBranches
                return $ newerBranch branches br
    tty <- hIsTerminalDevice stdin
    when (br /= Master) $ do
      shortlog <- git "log" ["HEAD.." ++ show prev, "--pretty=oneline"]
      unless (null shortlog) $ do
        putStrLn $ "Commits from " ++ show prev ++ ":"
        putStrLn $ simplifyCommitLog shortlog
        -- FIXME ignore Mass_Rebuild?
        -- FIXME check input is valid ref
        mref <- prompt "or ref to merge, or 'no' to skip merge"
        unless (map toLower mref == "no") $ do
          let ref = if null mref then show prev else mref
          git_ "merge" [ref]
    logs <- git "log" ["origin/" ++ show br ++ "..HEAD", "--pretty=oneline"]
    unless (null logs) $ do
      putStrLn "Local commits:"
      putStrLn $ simplifyCommitLog logs
      when tty $ prompt_ "to push and build"
      fedpkg_ "push" []
    nvr <- fedpkg "verrel" []
    buildstatus <- kojiBuildStatus nvr
    if buildstatus == COMPLETE
      then do
      putStrLn $ nvr ++ " is already built"
      buildBranch True (Just br) mpkg mock brs
      else do
      -- FIXME handle target
      latest <- cmd "koji" ["latest-build", "--quiet", branchDestTag br, pkg]
      if dropExtension nvr == dropExtension latest
        then putStrLn $ nvr ++ " is already latest"
        else do
        fedpkg_ "build" ["--fail-fast"]
        --waitForbuild
        (mbid,session) <- bzReviewSession
        if br == Master
          then forM_ mbid $ postBuild session nvr
          else do
          -- also query for open bugs
          let bugs = maybe [] (\b -> ["--bugs", show b]) mbid
          -- FIXME sometimes bodhi cli hangs or times out:
              -- check for successful update on 504 etc
          -- FIXME diff previous changelog?
          changelog <- getChangeLog $ pkg <.> "spec"
          putStrLn "Creating Bodhi Update..."
          -- FIXME check for autocreated update (pre-updates-testing)
          cmd_ "bodhi" (["updates", "new", "--type", if isJust mbid then "newpackage" else "enhancement", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
          -- override option
          when False $ cmd_ "bodhi" ["overrides", "save", nvr]
        buildBranch True (Just br) mpkg mock brs
  where
    postBuild session nvr bid = do
      let req = setRequestMethod "PUT" $
                setRequestCheckStatus $
                newBzRequest session ["bug", intAsText bid] [("cf_fixed_in", Just (T.pack nvr)), ("status", Just "MODIFIED")]
      void $ httpNoBody req
      putStrLn $ "build posted to review bug " ++ show bid

    postBranchReq url = do
      (mbid,session) <- bzReviewSession
      case mbid of
        Just bid -> do
          postComment session bid (T.pack url <> " (" <> T.pack (show br) <> ")")
          putStrLn $ "branch-request posted to review bug " ++ show bid
        Nothing -> putStrLn "no review bug found"

    checkNoBranchRequest :: Package -> IO ()
    checkNoBranchRequest pkg = do
      current <- cmdLines "pagure-cli" ["issues", "releng/fedora-scm-requests"]
      let reqs = filter (("New Branch \"" ++ show br ++ "\" for \"rpms/" ++ pkg ++ "\"") `isInfixOf`) current
      unless (null reqs) $
        error' $ "Request exists:\n" ++ unlines reqs

    mockConfig :: Branch -> String
    mockConfig Master = "fedora-rawhide-x86_64"
    mockConfig (Fedora n) = "fedora-" ++ show n ++ "-x86_64"

    simplifyCommitLog :: String -> String
    simplifyCommitLog = unlines . map (unwords . shortenHash . words) . lines
      where
        shortenHash :: [String] -> [String]
        shortenHash [] = []
        shortenHash (h:cs) = take 8 h : cs

getChangeLog :: FilePath -> IO String
getChangeLog spec = do
  clog <- cleanChangelog <$> cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
  putStrLn clog
  usrlog <- prompt "to use above or input the Update notes now"
  return $ if null usrlog then clog else usrlog
  where
    cleanChangelog cs =
      case length (lines cs) of
        0 -> error' "empty changelog" -- should not happen
        1 -> removePrefix "- " cs
        _ -> cs

brc :: T.Text
brc = "bugzilla.redhat.com"

postComment :: BugzillaSession -> BugId -> T.Text -> IO ()
postComment session bid comment = do
  let req = setRequestMethod "POST" $
            setRequestCheckStatus $
            newBzRequest session ["bug", intAsText bid, "comment"] [("comment", Just comment)]
  void $ newId . getResponseBody <$> httpJSON req
  putStrLn "Comment added:"
  T.putStrLn comment

getPackageDir :: IO String
getPackageDir = takeFileName <$> getCurrentDirectory

bzReviewSession :: IO (Maybe BugId,BugzillaSession)
bzReviewSession = do
  pkg <- getPackageDir
  (bids,session) <- bugIdsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bids of
    [bid] -> return (Just bid, session)
    _ -> return (Nothing, session)

bzLoginSession :: IO BugzillaSession
bzLoginSession = do
  ctx <- newBugzillaContext brc
  LoginSession ctx <$> getBzToken

bzSession :: Bool -> IO BugzillaSession
bzSession retry = do
  session <- bzLoginSession
  muser <- getBzUser
  let validreq = setRequestCheckStatus $
                 newBzRequest session ["valid_login"] [("login", muser)]
  valid <- validToken . getResponseBody <$> httpJSON validreq
  if not valid then do
    when retry $ error' "invalid login token"
    cmd_ "bugzilla" ["login"]
    putStrLn ""
    bzSession True
    else return session

packageReview :: SearchExpression
packageReview =
  ComponentField .==. ["Package Review"]

statusOpen :: SearchExpression
statusOpen =
  StatusField ./=. "CLOSED"

statusNewPost :: SearchExpression
statusNewPost =
  StatusField .==. "NEW" .||. StatusField .==. "ASSIGNED" .||. StatusField .==. "POST"

reviewApproved :: SearchExpression
reviewApproved =
  FlagsField `contains` "fedora-review+"

pkgReviews :: String -> SearchExpression
pkgReviews pkg =
  SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
  packageReview

bugIdsSession :: SearchExpression -> IO ([BugId],BugzillaSession)
bugIdsSession query = do
  session <- bzSession False
  bugs <- searchBugs' session query
  return (bugs, session)

bugsSession :: SearchExpression -> IO ([Bug],BugzillaSession)
bugsSession query = do
  session <- bzSession False
  bugs <- searchBugs session query
  return (bugs, session)

reviewBugIdSession :: String -> IO (BugId,BugzillaSession)
reviewBugIdSession pkg = do
  (bugs,session) <- bugIdsSession $ pkgReviews pkg .&&. statusOpen
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

approvedReviewBugIdSession :: String -> IO (BugId,BugzillaSession)
approvedReviewBugIdSession pkg = do
  (bugs,session) <- bugIdsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

requestRepo :: String -> IO ()
requestRepo pkg = do
  (bid,session) <- approvedReviewBugIdSession pkg
  putBugId bid
  created <- checkRepoCreatedComment session bid
  if created
    then putStrLn "scm repo was already created"
    else do
    -- show comments?
    checkNoRepoRequest
    url <- T.pack <$> fedpkg "request-repo" [pkg, show bid]
    T.putStrLn url
    let comment = T.pack "Thank you for the review\n\n" <> url
        req = setRequestMethod "POST" $
              setRequestCheckStatus $
              newBzRequest session ["bug", intAsText bid, "comment"] [("comment", Just comment)]
    void $ httpNoBody req
    putStrLn "comment posted"
  where
    checkNoRepoRequest :: IO ()
    checkNoRepoRequest = do
      -- FIXME also check pagure for repo
      current <- cmdLines "pagure-cli" ["issues", "releng/fedora-scm-requests"]
      let reqs = filter (("\"rpms/" ++ pkg ++ "\"") `isInfixOf`) current
      unless (null reqs) $
        error' $ "Request exists:\n" ++ unlines reqs

prompt :: String -> IO String
prompt s = do
  putStr $ "Press Enter " ++ s ++ ": "
  inp <- getLine
  putStrLn ""
  return inp

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

checkWorkingDirClean :: IO ()
checkWorkingDirClean = do
  clean <- gitBool "diff-index" ["--quiet", "HEAD"]
  unless clean $ error' "Working dir is not clean"

importPkgs :: [Package] -> IO ()
importPkgs [] = do
  pkgs <- map reviewBugToPackage <$> approvedReviews True
  mapM_ importPkg pkgs
importPkgs pkgs =
  mapM_ importPkg pkgs

reviewBugToPackage :: Bug -> String
reviewBugToPackage =
  head . words . removePrefix "Review Request: " . T.unpack . bugSummary

putPkgHdr :: String -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n== " ++ pkg ++ " =="

importPkg :: String -> IO ()
importPkg pkg = do
  putPkgHdr pkg
  dir <- getCurrentDirectory
  when (dir /= pkg) $ do
    direxists <- doesDirectoryExist pkg
    -- FIXME check repo exists
    unless direxists $ fedpkg_ "clone" [pkg]
    setCurrentDirectory pkg
    when direxists checkWorkingDirClean
  when (dir == pkg) checkWorkingDirClean
  (bid,session) <- approvedReviewBugIdSession pkg
  comments <- getComments session bid
  putStrLn ""
  putBugId bid
  mapM_ showComment comments
  prompt_ "to continue"
  let srpms = map (T.replace "/reviews//" "/reviews/") $ concatMap findSRPMs comments
  when (null srpms) $ error "No srpm urls found!"
  mapM_ T.putStrLn srpms
  let srpm = (head . filter isURI . filter (".src.rpm" `isSuffixOf`) . words . T.unpack . last) srpms
  let srpmfile = takeFileName srpm
  prompt_ $ "to import " ++ srpmfile
  havesrpm <- doesFileExist srpmfile
  unless havesrpm $
    cmd_ "curl" ["--silent", "--show-error", "--remote-name", srpm]
  -- check for krb5 ticket
  fedpkg_ "import" [srpmfile]
  git_ "commit" ["--message", "import #" ++ show bid]
  where
    findSRPMs :: Comment -> [T.Text]
    findSRPMs =
      filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText

showComment :: Comment -> IO ()
showComment cmt = do
  -- comment0 from fedora-create-review has leading newline
  T.putStr $ "(Comment " <> intAsText (commentCount cmt) <> ") <" <> commentCreator cmt <> "> " <> (T.pack . show) (commentCreationTime cmt)
            <> "\n\n" <> (T.unlines . map ("  " <>) . dropDuplicates . removeLeadingNewline . T.lines $ commentText cmt)
  putStrLn ""

newtype BzConfig = BzConfig {rcUserEmail :: UserEmail}
  deriving (Eq, Show)

getBzUser :: IO (Maybe UserEmail)
getBzUser = do
  home <- getEnv "HOME"
  let rc = home </> ".bugzillarc"
  readIniConfig rc rcParser rcUserEmail
  where
    rcParser :: IniParser BzConfig
    rcParser =
      section brc $ do
        user <- fieldOf "user" string
        return $ BzConfig user

newtype BzTokenConf = BzTokenConf {bzToken :: T.Text}
  deriving (Eq, Show)

getBzToken :: IO BugzillaToken
getBzToken = do
  cache <- getUserCacheFile "python-bugzilla" "bugzillatoken"
  res <- readIniConfig cache rcParser (BugzillaToken . bzToken)
  case res of
    Just token -> return token
    Nothing -> do
      cmd_ "bugzilla" ["login"]
      getBzToken
  where
    rcParser :: IniParser BzTokenConf
    rcParser =
      section brc $ do
        token <- fieldOf "token" string
        return $ BzTokenConf token

readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO (Maybe b)
readIniConfig inifile iniparser record = do
  havefile <- doesFileExist inifile
  if not havefile then return Nothing
    else do
    ini <- T.readFile inifile
    let config = parseIniFile ini iniparser
    return $ either error (Just . record) config

approvedCmd :: IO ()
approvedCmd =
  approvedReviews False >>= mapM_ putBug

approvedReviews :: Bool -> IO [Bug]
approvedReviews created = do
  session <- bzLoginSession
  muser <- getBzUser
  case muser of
    Nothing -> do
      putStrLn "Please login to bugzilla:"
      cmd_ "bugzilla" ["login"]
      approvedReviews created
    Just user -> do
      let query = ReporterField .==. user .&&. packageReview .&&.
                  statusNewPost .&&. reviewApproved
      bugs <- searchBugs session query
      let test = if created
                 then checkRepoCreatedComment session . bugId
                 else const (return True)
      filterM test bugs

checkRepoCreatedComment :: BugzillaSession -> BugId -> IO Bool
checkRepoCreatedComment session bid = do
    checkForComment session bid
      "(fedscm-admin):  The Pagure repository was created at"

checkForComment :: BugzillaSession -> BugId -> T.Text -> IO Bool
checkForComment session bid text = do
    comments <- map commentText <$> getComments session bid
    return $ any (text `T.isInfixOf`) $ reverse comments

listReviews :: IO ()
listReviews =
  openReviews >>= mapM_ putBug

openReviews :: IO [Bug]
openReviews = do
  session <- bzLoginSession
  muser <- getBzUser
  case muser of
    Nothing -> do
      putStrLn "Please login to bugzilla:"
      cmd_ "bugzilla" ["login"]
      openReviews
    Just user -> do
      let query = ReporterField .==. user .&&. packageReview .&&. statusNewPost
      searchBugs session query

putBug :: Bug -> IO ()
putBug bug = do
  putStrLn $ reviewBugToPackage bug
  putBugId $ bugId bug
  putStrLn ""

-- uniq for lists
dropDuplicates :: Eq a => [a] -> [a]
dropDuplicates (x:xs) =
  let ys = dropDuplicates xs in
    case ys of
      (y:_) | x == y -> ys
      _ -> x:ys
dropDuplicates _ = []

removeLeadingNewline :: [T.Text] -> [T.Text]
removeLeadingNewline ("":ts) = ts
removeLeadingNewline ts = ts

review :: String -> IO ()
review pkg = do
  (bugs, _) <- bugIdsSession $ pkgReviews pkg
  mapM_ putBugId bugs

putBugId :: BugId -> IO ()
putBugId =
  T.putStrLn . (("https://" <> brc <> "/show_bug.cgi?id=") <>) . intAsText

data KojiBuildStatus = COMPLETE | FAILED | BUILDING | NoBuild
  deriving (Eq, Read, Show)

kojiBuildStatus :: String -> IO KojiBuildStatus
kojiBuildStatus nvr = do
  mout <- cmdMaybe "koji" ["list-builds", "--quiet", "--buildid=" ++ nvr]
  case mout of
    Nothing -> return NoBuild
    Just out -> (return . read . last . words) out

findSpecfile :: IO FilePath
findSpecfile = fileWithExtension ".spec"
  where
    -- looks in dir for a unique file with given extension
    fileWithExtension :: String -> IO FilePath
    fileWithExtension ext = do
      files <- filter (\ f -> takeExtension f == ext) <$> getDirectoryContents "."
      maybe (error' ("No unique " ++ ext ++ " file found")) return $ listToMaybe files

-- FIXME assumed srpm in local dir
generateSrpm :: FilePath -> IO FilePath
generateSrpm spec = do
  nvr <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}-%{version}-%{release}", spec]
  let srpm = nvr <.> "src.rpm"
  haveSrpm <- doesFileExist srpm
  if haveSrpm then do
    specTime <- getModificationTime spec
    srpmTime <- getModificationTime srpm
    if srpmTime > specTime
      then do
      putStrLn $ srpm ++ " is up to date"
      return srpm
      else buildSrpm
    else buildSrpm
  where
    buildSrpm = do
      srpm <- takeFileName . last . words <$> cmd "rpmbuild" ["-bs", spec]
      putStrLn $ "Created " ++ srpm
      return srpm

createReview :: Maybe FilePath -> IO ()
createReview mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  unless (all isAscii pkg) $
    putStrLn "Warning: package name is not ASCII!"
  (bugs,session) <- bugsSession $ pkgReviews pkg
  unless (null bugs) $ do
    putStrLn "Existing review(s):"
    mapM_ putBug bugs
    prompt_ "to continue"
  srpm <- generateSrpm spec
  kojiurl <- kojiScratchBuild True srpm
  mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> do
      specSrpmUrls <- uploadPkgFiles fasid pkg spec srpm
      bugid <- postReviewReq session spec specSrpmUrls kojiurl pkg
      putStrLn "Review request posted:"
      putBugId bugid
  where
    postReviewReq :: BugzillaSession -> FilePath -> T.Text -> String -> String -> IO BugId
    postReviewReq session spec specSrpmUrls kojiurl pkg = do
      summary <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
      description <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
      let req = setRequestMethod "POST" $
              setRequestCheckStatus $
              newBzRequest session ["bug"]
              [ ("product", Just "Fedora")
              , ("component", Just "Package Review")
              , ("version", Just "rawhide")
              , ("summary", Just $ "Review Request: " <> T.pack pkg <> " - " <> summary)
              , ("description", Just $ specSrpmUrls <> "\n\nDescription:\n" <> description <> "\n\n\nKoji scratch build: " <> T.pack kojiurl)
              ]
      newId . getResponseBody <$> httpJSON req

getSpecFile :: Maybe FilePath -> IO String
getSpecFile =
  -- FIXME or change to dir
  maybe findSpecfile checkLocalFile
  where
    checkLocalFile :: FilePath -> IO FilePath
    checkLocalFile f =
      if takeFileName f == f then return f
        else error' "Please run in the directory of the spec file"

kojiScratchBuild :: Bool -> FilePath -> IO String
kojiScratchBuild failfast srpm = do
  out <- cmd "koji" $ ["build", "--scratch", "--nowait"] ++ ["--fail-fast" | failfast] ++ ["rawhide", srpm]
  putStrLn out
  let kojiurl = last $ words out
      task = takeWhileEnd (/= '=') kojiurl
  okay <- kojiWatchTask task
  if not okay
    then error' "scratch build failed"
    else return kojiurl
  where
    kojiWatchTask :: String -> IO Bool
    kojiWatchTask task = do
      res <- cmdBool "koji" ["watch-task", task]
      if res then return True
        else do
        ti <- kojiTaskInfo
        case ti of
          TaskClosed -> return True
          TaskFailed -> error "Task failed!"
          _ -> kojiWatchTask task
          where
            kojiTaskInfo :: IO TaskState
            kojiTaskInfo = do
              info <- cmdLines "koji" ["taskinfo", task]
              let state = removeStrictPrefix "State: " <$> filter ("State: " `isPrefixOf`) info
              return $
                case state of
                  ["open"] -> TaskOpen
                  ["failed"] -> TaskFailed
                  ["closed"] -> TaskClosed
                  ["free"] -> TaskFree
                  _ -> error "unknown task state!"

    takeWhileEnd :: (a -> Bool) -> [a] -> [a]
    takeWhileEnd p = reverse . takeWhile p . reverse

uploadPkgFiles :: String -> String -> FilePath -> FilePath -> IO T.Text
uploadPkgFiles fasid pkg spec srpm = do
  -- read ~/.config/fedora-create-review
  let sshhost = "fedorapeople.org"
      sshpath = "public_html/reviews/" ++ pkg
  cmd_ "ssh" [sshhost, "mkdir", "-p", sshpath]
  cmd_ "scp" [spec, srpm, sshhost ++ ":" ++ sshpath]
  getCheckedFileUrls $ "https://" <> fasid <> ".fedorapeople.org" </> removePrefix "public_html/" sshpath
  where
    getCheckedFileUrls :: String -> IO T.Text
    getCheckedFileUrls uploadurl = do
      let specUrl = uploadurl </> takeFileName spec
          srpmUrl = uploadurl </> takeFileName srpm
      mgr <- httpManager
      checkUrlOk mgr specUrl
      checkUrlOk mgr srpmUrl
      return $ "Spec URL: " <> T.pack specUrl <> "\nSRPM URL: " <> T.pack srpmUrl
      where
        checkUrlOk :: Manager -> String -> IO ()
        checkUrlOk mgr url = do
          okay <- httpExists mgr url
          unless okay $ error' $ "Could not access: " ++ url

data TaskState = TaskOpen | TaskFailed | TaskClosed | TaskFree

cmdT :: String -> [String] -> IO T.Text
cmdT c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> error' $ unwords (c:args) +-+ "failed with status" +-+ show n ++ "\n" ++ T.unpack err

pullPkgs :: [Package] -> IO ()
pullPkgs = mapM_ pullPkg

pullPkg :: String -> IO ()
pullPkg pkg =
  withExistingDirectory pkg $ do
    checkWorkingDirClean
    git_ "pull" ["--rebase"]

updateReview :: Bool -> Maybe FilePath -> IO ()
updateReview noscratch mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  (bid,session) <- reviewBugIdSession pkg
  putBugId bid
  srpm <- generateSrpm spec
  submitted <- checkForComment session bid (T.pack srpm)
  when submitted $
    error' "This NVR was already posted on the review bug: please bump"
  mkojiurl <-
    if noscratch
    then return Nothing
    else Just <$> kojiScratchBuild False srpm
  mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
  case mfasid of
    Nothing -> error' "Could not determine fasid from klist"
    Just fasid -> do
      specSrpmUrls <- uploadPkgFiles fasid pkg spec srpm
      changelog <- getChangeLog spec
      postComment session bid (specSrpmUrls <> (if null changelog then "" else "\n\n" <> T.pack changelog) <> maybe "" ("\n\nKoji scratch build: " <>) (T.pack <$> mkojiurl))
--      putStrLn "Review bug updated"
