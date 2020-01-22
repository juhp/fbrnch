import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs

import Control.Monad (void, when)
import Data.Ini.Config
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Simple
import System.Directory
--import System.Environment
import System.Environment.XDG.BaseDir
--import System.FilePath
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main =
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
  when tty $ do
    hSetBuffering stdout NoBuffering
    putStr "Press Enter to push"
    void getLine
  fedpkg "push" []
  -- check for existing build
  fedpkg "build" []
  --waitFor build
  --cmd_ "bodhi" ["updates", "new", build]
  --when override $ cmd_ "bodhi" ["overrides", "save", build]
  build mprev brs

brc :: T.Text
brc = "bugzilla.redhat.com"

bugsSession :: String -> IO ([BugId],BugzillaSession)
bugsSession pkg = do
  ctx <- newBugzillaContext brc
  token <- getBzToken
  let session = LoginSession ctx token
      query = SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
              ComponentField .==. ["Package Review"] .&&.
              StatusField ./=. "CLOSED" .&&.
              FlagsField `contains` "fedora-review+"
  bugs <- searchBugs' session query
  return (bugs, session)

requestRepo :: String -> IO ()
requestRepo pkg = do
  (bugs,session) <- bugsSession pkg
  case bugs of
    [] -> error' $ "no review bug found for " ++ pkg
    [pid] -> do
      T.putStrLn $ brc <> "/" <> intAsText pid
      -- show comments?
      url <- T.pack <$> cmd "fedpkg" ["request-repo", pkg, show pid]
      T.putStrLn url
      let comment = T.pack "Thank you for the review\n\n" <> url
          req = setRequestMethod "POST" $
                setRequestCheckStatus $
                newBzRequest session ["bug", intAsText pid, "comment"] [("comment", Just comment)]
      void $ httpNoBody req
      putStrLn "comment posted"
    _ -> error' "more than one review bug found!"

importPkg :: String -> IO ()
importPkg pkg = do
  (bugs,session) <- bugsSession pkg
  mapM_ (showComments session) bugs
  where
    showComments session bid = do
      cmts <- getComments session bid
      mapM_ T.putStrLn $ concatMap (filter (\ l -> "https://" `T.isInfixOf` l && not (any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"]) && "src.rpm" `T.isSuffixOf` l) . T.lines . commentText) cmts

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

