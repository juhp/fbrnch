{-# LANGUAGE CPP #-}

import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad
import Data.Ini.Config
import Data.Char (isDigit)
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
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

import Web.Bugzilla
import Web.Bugzilla.Search

data Branch = Fedora Int | Master
  deriving (Eq, Ord)

readBranch :: String -> Maybe Branch
readBranch "master" = Just Master
readBranch ('f':ns) | all isDigit ns && ((read ns :: Int) `elem` [30,31]) = Just $ Fedora (read ns)
readBranch unknown = error' $ "Unknown branch " ++ unknown

instance Show Branch where
  show Master = "master"
  show (Fedora n) = "f" ++ show n
--  show (EPEL n) = (if n <= 6 then "el" else "epel") ++ show n

latestBranch :: Branch
latestBranch = Fedora 31

newerBranch :: Branch -> Branch
newerBranch Master = Master
newerBranch (Fedora n) | Fedora n >= latestBranch = Master
newerBranch (Fedora n) = Fedora (n+1)

olderBranch :: Branch -> Branch
olderBranch Master = latestBranch
olderBranch (Fedora n) = Fedora (n-1)

type Package = String

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "approved" "List approved reviews" $
      pure approved
    , Subcommand "build-branch" "Build branch(s) of package" $
      buildBranch Nothing Nothing <$> noMockOpt <*> some branchArg
    , Subcommand "build" "Build package(s)" $
      build <$> noMockOpt <*> branchOpt <*> some pkgArg
    , Subcommand "request" "Request dist git repo for new package" $
      requestRepo <$> strArg "NEWPACKAGE"
    , Subcommand "review" "Package review for package" $
      review <$> strArg "PACKAGE"
    , Subcommand "import" "Import new package via bugzilla" $
      importPkg <$> strArg "NEWPACKAGE"
    ]
  where
    branchArg :: Parser Branch
    branchArg = argumentWith (maybeReader readBranch) "BRANCH.."

    branchOpt :: Parser (Maybe Branch)
    branchOpt = optional (optionWith (maybeReader readBranch) 'b' "branch" "BRANCH" "branch")

    pkgArg :: Parser Package
    pkgArg = removeSuffix "/" <$> strArg "PACKAGE.."

    noMockOpt = switchWith 'n' "no-mock" "Do not use mock to test branch"

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

build :: Bool -> Maybe Branch -> [Package] -> IO ()
build _ _ [] = return ()
build noMock mbr (pkg:pkgs) = do
  -- FIXME active branches determination
  let branches = maybe [Master, latestBranch, olderBranch latestBranch] pure mbr
  withCurrentDirectory pkg $ buildBranch Nothing (Just pkg) noMock branches
  build noMock mbr pkgs

buildBranch :: Maybe Branch -> Maybe Package -> Bool -> [Branch] -> IO ()
buildBranch _ _ _ [] = return ()
buildBranch mprev mpkg noMock (br:brs) = do
  checkWorkingDirClean
  git_ "pull" []
  pkg <- maybe getPackageDir return mpkg
  branched <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
  if not branched then
    if br == Master
    then error' "no origin/master found!"
    else do
      unless noMock $ fedpkg_ "mockbuild" ["--root", mockConfig br]
      checkNoBranchRequest pkg
      putStrLn $ "requesting branch " ++ show br
      -- FIXME? request all branches? or mock first?
      url <- fedpkg "request-branch" [show br]
      putStrLn url
      postBranchReq url
    else do
    current <- git "rev-parse" ["--abbrev-ref", "HEAD"]
    when (current /= show br) $
      fedpkg_ "switch-branch" ["--fetch", show br]
    let prev = fromMaybe (newerBranch br) mprev
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
      then buildBranch (Just br) mpkg noMock brs
      else do
      -- FIXME handle master branch and target
      latest <- cmd "koji" ["latest-build", "--quiet", show br ++ "-updates-candidate", pkg]
      if dropExtension nvr == dropExtension latest
        then putStrLn $ nvr ++ " is already latest"
        else do
        fedpkg_ "build" ["--fail-fast"]
        --waitForbuild
        (mbid,session) <- bugSessionPkg
        if br == Master
          then forM_ mbid $ postBuild session nvr
          else do
          let bugs = maybe [] (\b -> ["--bugs", show b]) mbid
          -- FIXME sometimes bodhi cli hangs
          -- FIXME detect version update/include changelog
          cmd_ "bodhi" (["updates", "new", "--type", if isJust mbid then "newpackage" else "enhancement", "--notes", nvr, "--autokarma", "--autotime"] ++ bugs ++ [nvr])
          -- override option
          when False $ cmd_ "bodhi" ["overrides", "save", nvr]
        buildBranch (Just br) mpkg noMock brs
  where
    postBuild session nvr bid = do
      let req = setRequestMethod "POST" $
                setRequestCheckStatus $
                newBzRequest session ["bug", intAsText bid] [("cf_fixed_in", Just (T.pack nvr)), ("status", Just "MODIFIED")]
      void $ httpNoBody req
      putStrLn $ "build posted to review bug " ++ show bid

    postBranchReq url = do
      (mbid,session) <- bugSessionPkg
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

brc :: T.Text
brc = "bugzilla.redhat.com"

getPackageDir :: IO String
getPackageDir = takeFileName <$> getCurrentDirectory

bugSessionPkg :: IO (Maybe BugId,BugzillaSession)
bugSessionPkg = do
  pkg <- getPackageDir
  (bids,session) <- bugsSession False pkg
  case bids of
    [bid] -> return (Just bid, session)
    _ -> return (Nothing, session)

bugsSession :: Bool -> String -> IO ([BugId],BugzillaSession)
bugsSession retry pkg = do
  ctx <- newBugzillaContext brc
  token <- getBzToken
  muser <- getBzUser
  let session = LoginSession ctx token
      validreq = setRequestCheckStatus $
                 newBzRequest session ["valid_login"] [("login", muser)]
  valid <- getResponseBody <$> httpLBS validreq
  if valid == "false" then do
    when retry $ error' "invalid login token"
    cmd_ "bugzilla" ["login"]
    bugsSession True pkg
    else do
    let query = SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
                ComponentField .==. "Package Review" .&&.
                StatusField ./=. "CLOSED" .&&.
                FlagsField `contains` "fedora-review+"
    bugs <- searchBugs' session query
    return (bugs, session)

bugSession :: String -> IO (BugId,BugzillaSession)
bugSession pkg = do
  (bugs,session) <- bugsSession False pkg
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

requestRepo :: String -> IO ()
requestRepo pkg = do
  (bid,session) <- bugSession pkg
  putBug bid
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

importPkg :: String -> IO ()
importPkg pkg = do
  dir <- getCurrentDirectory
  when (dir /= pkg) $ do
    direxists <- doesDirectoryExist pkg
    unless direxists $ fedpkg_ "clone" [pkg]
    setCurrentDirectory pkg
    when direxists checkWorkingDirClean
  when (dir == pkg) checkWorkingDirClean
  (bid,session) <- bugSession pkg
  comments <- getComments session bid
  putStrLn ""
  putBug bid
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
approved = do
  ctx <- newBugzillaContext brc
  token <- getBzToken
  muser <- getBzUser
  case muser of
    Nothing -> do
      putStrLn "Please login to bugzilla:"
      cmd_ "bugzilla" ["login"]
      approved
    Just user -> do
      let session = LoginSession ctx token
          query = ReporterField .==. user .&&.
                  ComponentField .==. "Package Review" .&&.
                  StatusField ./=. "CLOSED" .&&.
                  FlagsField `contains` "fedora-review+"
      -- FIXME use searchBugs to extract package names
      bugs <- searchBugs' session query
      mapM_ putBug bugs

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
  (bugs, _) <- bugsSession False pkg
  mapM_ putBug bugs

putBug :: BugId -> IO ()
putBug =
  T.putStrLn . (("https://" <> brc <> "/show_bug.cgi?id=") <>) . intAsText

data KojiBuildStatus = COMPLETE | FAILED | BUILDING | NoBuild
  deriving (Eq, Read, Show)

kojiBuildStatus :: String -> IO KojiBuildStatus
kojiBuildStatus nvr = do
  mout <- cmdMaybe "koji" ["list-builds", "--quiet", "--buildid=" ++ nvr]
  case mout of
    Nothing -> return NoBuild
    Just out -> (return . read . last . words) out
