{-# LANGUAGE CPP #-}

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad
import Data.Char (isAscii)
import Data.Ini.Config
import Data.List
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

import NewBug

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
    [ Subcommand "create" "Create a Package Review request" $
      createReview <$> strArg "SPECFILE"
    , Subcommand "approved" "List approved reviews" $
      pure approved
    , Subcommand "request" "Request dist git repo for new package" $
      requestRepo <$> strArg "NEWPACKAGE"
    , Subcommand "import" "Import new package via bugzilla" $
      importPkgs <$> many (strArg "NEWPACKAGE...")
    , Subcommand "build" "Build package(s)" $
      build <$> mockOpt <*> branchOpt <*> some pkgArg
    , Subcommand "build-branch" "Build branch(s) of package" $
      buildBranch Nothing <$> pkgOpt <*> mockOpt <*> some branchArg
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
    branches <- case mbr of
      Just b | b `elem` fedBranches -> return [b]
             | otherwise -> error' "Unsupported branch"
      Nothing ->
        -- FIXME problem is we may want --all: maybe better just to request --all
        filter (`elem` fedBranches) <$> getPackageBranches
    buildBranch Nothing (Just pkg) mock branches
  build mock mbr pkgs

buildBranch :: Maybe Branch -> Maybe Package -> Bool -> [Branch] -> IO ()
buildBranch _ _ _ [] = return ()
buildBranch mprev mpkg mock (br:brs) = do
  checkWorkingDirClean
  git_ "pull" []
  pkg <- maybe getPackageDir return mpkg
  branched <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
  if not branched then
    if br == Master
    then error' "no origin/master found!"
    else do
      when mock $ fedpkg_ "mockbuild" ["--root", mockConfig br]
      checkNoBranchRequest pkg
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
    git_ "log" ["HEAD.." ++ show prev, "--pretty=oneline"]
    when (br /= Master) $ do
      merged <- gitBool "diff" ["--quiet", show br, show prev]
      unless merged $ do
        -- FIXME ignore Mass_Rebuild
        mref <- prompt "or ref to merge, or no: "
        unless (mref == "no") $ do
          let ref = if null mref then show prev else mref
          git_ "merge" [ref]
    logs <- git "log" ["origin/" ++ show br ++ "..HEAD", "--pretty=oneline"]
    unless (null logs) $ do
      git_ "log" ["origin/" ++ show br ++ "..HEAD", "--pretty=oneline"]
      when tty $ prompt_ "to push"
      fedpkg_ "push" []
    nvr <- fedpkg "verrel" []
    buildstatus <- kojiBuildStatus nvr
    if buildstatus == COMPLETE
      then buildBranch (Just br) mpkg mock brs
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
          changelog <- cleanChangelog <$> cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", pkg <.> "spec"]
          putStrLn "Creating Bodhi Update..."
          -- FIXME check for autocreated update (pre-updates-testing)
          cmd_ "bodhi" (["updates", "new", "--type", if isJust mbid then "newpackage" else "enhancement", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
          -- override option
          when False $ cmd_ "bodhi" ["overrides", "save", nvr]
        buildBranch (Just br) mpkg mock brs
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
          let req = setRequestMethod "POST" $
                    setRequestCheckStatus $
                    newBzRequest session ["bug", intAsText bid, "comment"] [("comment", Just (T.pack url <> " (" <> T.pack (show br) <> ")"))]
          void $ httpNoBody req
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

    cleanChangelog cs =
      case length (lines cs) of
        0 -> error' "empty changelog" -- should not happen
        1 -> removePrefix "- " cs
        _ -> cs

brc :: T.Text
brc = "bugzilla.redhat.com"

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
  valid <- getResponseBody <$> httpLBS validreq
  if valid == "false" then do
    when retry $ error' "invalid login token"
    cmd_ "bugzilla" ["login"]
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
  StatusField .==. "NEW" .||. StatusField .==. "POST"

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

bugIdSession :: String -> IO (BugId,BugzillaSession)
bugIdSession pkg = do
  (bugs,session) <- bugIdsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

requestRepo :: String -> IO ()
requestRepo pkg = do
  (bid,session) <- bugIdSession pkg
  putBugId bid
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
      -- FIXME also check for repo or closed ticket
      current <- cmdLines "pagure-cli" ["issues", "releng/fedora-scm-requests"]
      let reqs = filter (("\"rpms/" ++ pkg ++ "\"") `isInfixOf`) current
      unless (null reqs) $
        error' $ "Request exists:\n" ++ unlines reqs

prompt :: String -> IO String
prompt s = do
  putStr $ "Press Enter " ++ s
  getLine

prompt_ :: String -> IO ()
prompt_ = void <$> prompt

checkWorkingDirClean :: IO ()
checkWorkingDirClean = do
  clean <- gitBool "diff-index" ["--quiet", "HEAD"]
  unless clean $ error' "Working dir is not clean"

importPkgs :: [String] -> IO ()
importPkgs [] = do
  -- FIXME check repo creation done
  pkgs <- map reviewBugToPackage <$> approvedReviews
  mapM_ importPkg pkgs
importPkgs pkgs =
  mapM_ importPkg pkgs

reviewBugToPackage :: Bug -> String
reviewBugToPackage =
  head . words . removePrefix "Review Request: " . T.unpack . bugSummary

importPkg :: String -> IO ()
importPkg pkg = do
  dir <- getCurrentDirectory
  when (dir /= pkg) $ do
    direxists <- doesDirectoryExist pkg
    -- FIXME check repo exists
    unless direxists $ fedpkg_ "clone" [pkg]
    setCurrentDirectory pkg
    when direxists checkWorkingDirClean
  when (dir == pkg) checkWorkingDirClean
  (bid,session) <- bugIdSession pkg
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

approved :: IO ()
approved =
  approvedReviews >>= mapM_ putBug

approvedReviews :: IO [Bug]
approvedReviews = do
  session <- bzLoginSession
  muser <- getBzUser
  case muser of
    Nothing -> do
      putStrLn "Please login to bugzilla:"
      cmd_ "bugzilla" ["login"]
      approvedReviews
    Just user -> do
      let query = ReporterField .==. user .&&. packageReview .&&.
                  statusNewPost .&&. reviewApproved
      searchBugs session query

listReviews :: IO ()
listReviews = do
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
  -- FIXME remove prefix "Review Request: "?
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

createReview :: FilePath -> IO ()
createReview spec = do
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  unless (all isAscii pkg) $
    putStrLn "Warning: package name is not ASCII!"
  (bugs,session) <- bugsSession $ pkgReviews pkg
  unless (null bugs) $ do
    putStrLn "Existing review(s):"
    mapM_ putBug bugs
    prompt_ "to continue"
  -- FIXME or check existing srpm newer than spec file
  -- FIXME build srpm in/from spec file dir
  srpm <- last . words <$> cmd "rpmbuild" ["-bs", spec]
  putStrLn srpm
  out <- cmd "koji" ["build", "--scratch", "--nowait", "--fail-fast", "rawhide", srpm]
  let kojiurl = last $ words out
      task = takeWhileEnd (/= '=') kojiurl
  okay <- kojiWatchTask task
  if not okay then error' "scratch build failed"
    else do
    mfasid <- (removeSuffix "@FEDORAPROJECT.ORG" <$>) . find ("@FEDORAPROJECT.ORG" `isSuffixOf`) . words <$> cmd "klist" ["-l"]
    case mfasid of
      Nothing -> error' "Could not determine fasid from klist"
      Just fasid -> do
        let sshhost = "fedorapeople.org"
            sshpath = "public_html/reviews/" ++ pkg
        cmd_ "ssh" [sshhost, "mkdir", "-p", sshpath]
        cmd_ "scp" [spec, srpm, sshhost ++ ":" ++ sshpath]
        bugid <- postReviewReq session srpm fasid kojiurl pkg
        putStrLn "Review request posted:"
        putBugId bugid
  where
    takeWhileEnd p = reverse . takeWhile p . reverse

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

    postReviewReq :: BugzillaSession -> FilePath -> String -> String -> String -> IO BugId
    postReviewReq session srpm fasid kojiurl pkg = do
      summary <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
      description <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
      -- read ~/.config/fedora-create-review
      -- FIXME share path with sshpath
      -- FIXME test urls exist
      let url = "https://" <> fasid <> ".fedorapeople.org/reviews" </> pkg
          req = setRequestMethod "POST" $
              setRequestCheckStatus $
              newBzRequest session ["bug"]
              [ ("product", Just "Fedora")
              , ("component", Just "Package Review")
              , ("version", Just "rawhide")
              , ("summary", Just $ "Review Request: " <> T.pack pkg <> " - " <> summary)
              , ("description", Just $ "Spec URL: " <> T.pack (url </> takeFileName spec) <> "\nSRPM URL: " <> T.pack (url </> takeFileName srpm) <> "\n\nDescription:\n" <> description <> "\n\n\nKoji scratch build: " <> T.pack kojiurl)
              ]
      newBugId . getResponseBody <$> httpJSON req

data TaskState = TaskOpen | TaskFailed | TaskClosed | TaskFree

cmdT :: String -> [String] -> IO T.Text
cmdT c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> error' $ unwords (c:args) +-+ "failed with status" +-+ show n ++ "\n" ++ T.unpack err

pullPkgs :: [String] -> IO ()
pullPkgs pkgs = mapM_ pullPkg pkgs

pullPkg :: String -> IO ()
pullPkg pkg = do
  withExistingDirectory pkg $ do
    checkWorkingDirClean
    git_ "pull" ["--rebase"]
