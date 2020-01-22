import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad
import Data.Ini.Config
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Simple
import Network.URI (isURI)
import System.Directory
--import System.Environment
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  simpleCmdArgs Nothing "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "build" "Build for branches" $
      build Nothing <$> some (strArg "BRANCH...")
    , Subcommand "request" "Request dist git repo for new package" $
      requestRepo <$> strArg "NEWPACKAGE"
    , Subcommand "import" "Import new package via bugzilla" $
      importPkg <$> strArg "NEWPACKAGE"
    ]

fedpkg :: String -> [String] -> IO ()
fedpkg c args =
  cmd_ "fedpkg" (c:args)

build :: Maybe String -> [String] -> IO ()
build _ [] = return ()
build mprev (br:brs) = do
  fedpkg "switch-branch" [br]
  let prev = fromMaybe "master" mprev
  when (br /= "master") $ do
    git_ "diff" [br, prev]
    git_ "merge" [prev]
  tty <- hIsTerminalDevice stdin
  when tty $ prompt "push"
  fedpkg "push" []
  -- check for existing build
  fedpkg "build" []
  --waitFor build
  --cmd_ "bodhi" ["updates", "new", build]
  --when override $ cmd_ "bodhi" ["overrides", "save", build]
  build mprev brs

brc :: T.Text
brc = "bugzilla.redhat.com"

bugSession :: String -> IO (BugId,BugzillaSession)
bugSession pkg = do
  ctx <- newBugzillaContext brc
  token <- getBzToken
  let session = LoginSession ctx token
      query = SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
              ComponentField .==. ["Package Review"] .&&.
              StatusField ./=. "CLOSED" .&&.
              FlagsField `contains` "fedora-review+"
  bugs <- searchBugs' session query
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

requestRepo :: String -> IO ()
requestRepo pkg = do
  (bid,session) <- bugSession pkg
  T.putStrLn $ brc <> "/" <> intAsText bid
  -- show comments?
  url <- T.pack <$> cmd "fedpkg" ["request-repo", pkg, show bid]
  T.putStrLn url
  let comment = T.pack "Thank you for the review\n\n" <> url
      req = setRequestMethod "POST" $
            setRequestCheckStatus $
            newBzRequest session ["bug", intAsText bid, "comment"] [("comment", Just comment)]
  void $ httpNoBody req
  putStrLn "comment posted"

prompt :: String -> IO ()
prompt s = do
  putStr $ "Press Enter to " ++ s
  void getLine

importPkg :: String -> IO ()
importPkg pkg = do
  dir <- getCurrentDirectory
  when (dir /= pkg) $ do
    direxists <- doesDirectoryExist pkg
    unless direxists $ fedpkg "clone" [pkg]
    setCurrentDirectory pkg
  (bid,session) <- bugSession pkg
  comments <- getComments session bid
  putStrLn ""
  T.putStrLn $ "https://" <> brc <> "/" <> intAsText bid
  mapM_ showComment comments
  prompt "continue"
  let srpms = concatMap findSRPMs comments
  when (null srpms) $ error "No srpm urls found!"
  mapM_ T.putStrLn srpms
  let srpm = (head . filter isURI . filter (".src.rpm" `isSuffixOf`) . words . T.unpack . last) srpms
  let srpmfile = takeFileName srpm
  prompt $ "import " ++ srpmfile
  cmd_ "curl" ["-O", srpm]
  fedpkg "import" [srpmfile]
  git_ "commit" ["import #" ++ show bid]
  where
    findSRPMs :: Comment -> [T.Text]
    findSRPMs =
      filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText

showComment :: Comment -> IO ()
showComment cmt = do
  T.putStr $ "(Comment " <> intAsText (commentCount cmt) <> ") <" <> commentCreator cmt <> "> " <> (T.pack . show) (commentCreationTime cmt)
            <> "\n" <> (T.unlines . map ("  " <>) . T.lines $ commentText cmt)
  putStrLn ""

-- newtype BzConfig = BzConfig {rcUserEmail :: UserEmail}
--   deriving (Eq, Show)

-- getBzUser :: IO (Maybe UserEmail)
-- getBzUser = do
--   home <- getEnv "HOME"
--   let rc = home </> ".bugzillarc"
--   readIniConfig rc rcParser rcUserEmail
--   where
--     rcParser :: IniParser BzConfig
--     rcParser =
--       section brc $ do
--         user <- fieldOf "user" string
--         return $ BzConfig user

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

