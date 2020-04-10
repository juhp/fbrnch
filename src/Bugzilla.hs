module Bugzilla where

import Common
import Common.System
import qualified Common.Text as T

import Data.Ini.Config
import Network.HTTP.Simple
import System.Environment
import System.Environment.XDG.BaseDir
import Web.Bugzilla
import Web.Bugzilla.Search
-- local
import Bugzilla.NewId
import Bugzilla.ValidLogin
import Package (getPackageName)

postBuildComment :: BugzillaSession -> String -> BugId -> IO ()
postBuildComment session nvr bid = do
  let req = setRequestMethod "PUT" $
            setRequestCheckStatus $
            newBzRequest session ["bug", intAsText bid] [("cf_fixed_in", Just (T.pack nvr)), ("status", Just "MODIFIED")]
  void $ httpNoBody req
  putStrLn $ "build posted to review bug " ++ show bid

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

bzReviewSession :: IO (Maybe BugId,BugzillaSession)
bzReviewSession = do
  pkg <- getPackageName Nothing
  (bids,session) <- bugIdsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bids of
    [bid] -> return (Just bid, session)
    _ -> return (Nothing, session)

bzLoginSession :: IO (BugzillaSession, UserEmail)
bzLoginSession = do
  user <- getBzUser
  ctx <- newBugzillaContext brc
  session <- LoginSession ctx <$> getBzToken
  let validreq = setRequestCheckStatus $
                 newBzRequest session ["valid_login"] [("login", Just user)]
  valid <- validToken . getResponseBody <$> httpJSON validreq
  if not valid
    then do
    putStrLn "No valid bugzilla login token, please login:"
    cmd_ "bugzilla" ["login"]
    bzLoginSession
    else return (session,user)
  where
    getBzUser :: IO UserEmail
    getBzUser = do
      home <- getEnv "HOME"
      let rc = home </> ".bugzillarc"
      muser <- readIniConfig rc rcParser rcUserEmail
      case muser of
        Nothing -> do
          putStrLn "Please login to bugzilla:"
          cmd_ "bugzilla" ["login"]
          getBzUser
        Just user -> return user
      where
        rcParser :: IniParser BzConfig
        rcParser =
          section brc $ do
            user <- fieldOf "user" string
            return $ BzConfig user

reporterIs :: T.Text -> SearchExpression
reporterIs = (ReporterField .==.)

packageReview :: SearchExpression
packageReview =
  ComponentField .==. ["Package Review"]

statusOpen :: SearchExpression
statusOpen =
  StatusField ./=. "CLOSED"

statusNewPost :: SearchExpression
statusNewPost =
  StatusField .==. "NEW" .||. StatusField .==. "ASSIGNED" .||. StatusField .==. "POST"

statusNewModified :: SearchExpression
statusNewModified =
  StatusField .==. "NEW" .||. StatusField .==. "ASSIGNED" .||. StatusField .==. "POST" .||. StatusField .==. "MODIFIED"

reviewApproved :: SearchExpression
reviewApproved =
  FlagsField `contains` "fedora-review+"

pkgReviews :: String -> SearchExpression
pkgReviews pkg =
  SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
  packageReview

bugIdsSession :: SearchExpression -> IO ([BugId],BugzillaSession)
bugIdsSession query = do
  (session,_) <- bzLoginSession
  bugs <- searchBugs' session query
  return (bugs, session)

bugsSession :: SearchExpression -> IO ([Bug],BugzillaSession)
bugsSession query = do
  (session,_) <- bzLoginSession
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

reviewBugToPackage :: Bug -> String
reviewBugToPackage =
  head . words . removePrefix "Review Request: " . T.unpack . bugSummary

readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO (Maybe b)
readIniConfig inifile iniparser record = do
  havefile <- doesFileExist inifile
  if not havefile then return Nothing
    else do
    ini <- T.readFile inifile
    let config = parseIniFile ini iniparser
    return $ either error (Just . record) config

bugStatusEnum :: T.Text -> Int
bugStatusEnum st =
  case st of
    "NEW" -> 0
    "ASSIGNED" -> 1
    "POST" -> 2
    "MODIFIED" -> 3
    "ON_QA" -> 4
    "VERIFIED" -> 5
    "RELEASE_PENDING" -> 6
    "CLOSED" -> 7
    _ -> -1

showComment :: Comment -> IO ()
showComment cmt = do
  -- comment0 from fedora-create-review has leading newline
  T.putStr $ "(Comment " <> intAsText (commentCount cmt) <> ") <" <> commentCreator cmt <> "> " <> (T.pack . show) (commentCreationTime cmt)
            <> "\n\n" <> (T.unlines . map ("  " <>) . dropDuplicates . removeLeadingNewline . T.lines $ commentText cmt)
  putStrLn ""

newtype BzConfig = BzConfig {rcUserEmail :: UserEmail}
  deriving (Eq, Show)

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

checkRepoCreatedComment :: BugzillaSession -> BugId -> IO Bool
checkRepoCreatedComment session bid =
    checkForComment session bid
      "(fedscm-admin):  The Pagure repository was created at"

checkForComment :: BugzillaSession -> BugId -> T.Text -> IO Bool
checkForComment session bid text = do
    comments <- map commentText <$> getComments session bid
    return $ any (text `T.isInfixOf`) $ reverse comments

putBug :: Bug -> IO ()
putBug bug = do
  putStrLn $ reviewBugToPackage bug ++ " (" ++ T.unpack (bugStatus bug) ++ ")"
  putBugId $ bugId bug
  putStrLn ""

putBugId :: BugId -> IO ()
putBugId =
  T.putStrLn . (("https://" <> brc <> "/show_bug.cgi?id=") <>) . intAsText

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

testBZlogin :: IO ()
testBZlogin =
  void bzLoginSession
