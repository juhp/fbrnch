{-# LANGUAGE OverloadedStrings #-}

module Bugzilla (
  Bug(..),
  User(..),
  BugId,
  -- session
  BugzillaSession,
  bugIdsSession,
  bugsSession,
  bzLoginSession,
  bzReviewSession,
  reviewBugIdSession,
  approvedReviewBugIdSession,
  approvedReviewBugSession,
  pkgBugs,
  pkgReviews,
  listBzUsers,
  emailIsValid,
--  testBZlogin,
  -- search
  searchBugs,
  (.&&.),
  not',
  packageReview,
  assigneeIs,
  bugIdIs,
  reporterIs,
  reviewApproved,
  statusNewAssigned,
  statusNewPost,
  statusNewModified,
  statusOpen,
  summaryContains,
  -- comments
  Comment,
  checkForComment,
  checkRepoCreatedComment,
  createBug,
  commentBug,
  updateBug,
  putBugBuild,
  showComment,
  commentText,
  getComments,
  --
  reviewBugToPackage,
  sortBugsByProduct,
  sortBugsByStatus,
  -- output
  putBug,
  putBugVer,
  putReviewBug,
  putBugId,
  -- request
  newBzRequest,
  intAsText,
  makeTextItem
  ) where

import Common
import Common.System
import qualified Common.Text as T
import Prompt

import Control.Exception (finally)
import Data.Aeson.Types (Array, Object)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8
import Data.Ini.Config
import Network.HTTP.Conduit
import Network.HTTP.Query
import Network.HTTP.Simple
import System.Environment
import System.Environment.XDG.BaseDir
import System.IO (hSetEcho, stdin)
import qualified Text.Email.Validate as Email
import Web.Bugzilla.RedHat
import Web.Bugzilla.RedHat.Search

createBug :: BugzillaSession -> [(String,String)] -> IO Int
createBug session params = do
  let req = -- posting url encoded utf8 to bugzilla only seems to work in body
            urlEncodedBody (encodeParams params) $
            setRequestCheckStatus $
            newBzRequest session ["bug"] []
  lookupKey' "id" . getResponseBody <$> httpJSON req

commentBug :: BugzillaSession -> BugId -> String -> IO ()
commentBug session bid comment = do
  let req = setRequestMethod "POST" $
            -- earlier posting url encoded utf8 only seemed to work in body
            urlEncodedBody (encodeParams [("comment", comment)]) $
            setRequestCheckStatus $
            newBzRequest session (map T.pack ["bug",show bid,"comment"]) []
  res <- getResponseBody <$> httpJSON req
  -- [("id",Number 1.4682731e7)]
  when (isNothing (lookupKey "id" res :: Maybe Int)) $ do
    -- eg [("error",Bool True),("documentation",String "https://bugzilla.redhat.com/docs/en/html/api/index.html"),("code",Number 32614.0),("message",String "A REST API resource was not found for 'POST /bug/1880903'.")]
    case lookupKey "message" res of
      Nothing -> print res
      Just msg -> T.putStrLn msg
    error' $ "failed to update bug " ++ show bid
  putStrLn "Comment added:"
  putStrLn comment

updateBug :: BugzillaSession -> BugId -> [(String,String)] -> IO ()
updateBug session bid params = do
  let req = setRequestMethod "PUT" $
            -- earlier posting url encoded utf8 only seemed to work in body
            urlEncodedBody (encodeParams params) $
            setRequestCheckStatus $
            newBzRequest session (map T.pack ["bug",show bid]) []
  res <- getResponseBody <$> httpJSON req
  -- [("bugs",Array [Object (fromList [("changes",Object (fromList [])),("alias",Array []),("id",Number 1897441.0),("last_change_time",String "2021-01-24T08:20:18Z")])])]
  when (isNothing (lookupKey "bugs" res :: Maybe Array)) $ do
    -- eg [("error",Bool True),("documentation",String "https://bugzilla.redhat.com/docs/en/html/api/index.html"),("code",Number 32614.0),("message",String "A REST API resource was not found for 'POST /bug/1880903'.")]
    case lookupKey "message" res of
      Nothing -> print res
      Just msg -> T.putStrLn msg
    error' $ "failed to update bug " ++ show bid

encodeParams :: [(String, String)] -> [(ByteString, ByteString)]
encodeParams [] = []
encodeParams ((k,v):ps) =
  (B.pack k, fromString v) : encodeParams ps

putBugBuild :: BugzillaSession -> BugId -> String -> IO ()
putBugBuild session bid nvr = do
  void $ updateBug session bid
    [("cf_fixed_in", nvr), ("status", "MODIFIED")]
  putStrLn $ "build posted to review bug " ++ show bid

brc :: T.Text
brc = "bugzilla.redhat.com"

bzReviewSession :: IO (Maybe BugId,BugzillaSession)
bzReviewSession = do
  pkg <- getDirectoryName
  (bids,session) <- bugIdsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bids of
    [bid] -> return (Just bid, session)
    _ -> return (Nothing, session)

newtype BzUserRC = BzUserRC {rcUserEmail :: UserEmail}
  deriving (Eq, Show)

emailIsValid :: String -> Bool
emailIsValid = Email.isValid . B.pack

-- FIXME support bugzilla API key
bzLoginSession :: IO (BugzillaSession, UserEmail)
bzLoginSession = do
  user <- getBzUser
  ctx <- newBugzillaContext brc
  session <- getBzLoginSession ctx user
  return (session,user)
  where
    getBzUser :: IO UserEmail
    getBzUser = do
      home <- getEnv "HOME"
      let rc = home </> ".bugzillarc"
      -- FIXME assumption if file exists then it has b.r.c user
      ifM (doesFileExist rc)
        (readIniConfig rc rcParser rcUserEmail) $
        do
        -- FIXME: option to override email
        email <- prompt "Bugzilla Username"
        when (emailIsValid email) $ do
          T.writeFile rc $ "[" <> brc <> "]\nuser = " <> T.pack email <> "\n"
          putStrLn $ "Saved in " ++ rc
        getBzUser
      where
        rcParser :: IniParser BzUserRC
        rcParser =
          section brc $
          BzUserRC <$> fieldOf "user" string

    getBzLoginSession :: BugzillaContext -> UserEmail -> IO BugzillaSession
    getBzLoginSession ctx user = do
      cache <- getUserCacheFile "python-bugzilla" "bugzillatoken"
      let cacheDir = takeDirectory cache
      cacheDirExists <- doesDirectoryExist cacheDir
      unless cacheDirExists $ createDirectory cacheDir
      tokenstatus <- ifM (notM (doesFileExist cache)) (return NoToken) $
        do
        token <- readIniConfig cache rcParser bzToken
        let session = LoginSession ctx $ BugzillaToken token
        let validreq = setRequestCheckStatus $
                       newBzRequest session ["valid_login"] [("login",Just user)]
        valid <- lookupKey' "result" . getResponseBody <$> httpJSON validreq
        return $ if valid then ValidToken session else InvalidToken token
      case tokenstatus of
        ValidToken session -> return session
        InvalidToken oldtoken -> do
          token <- bzLogin
          cmd_ "sed" ["-i", "s/" ++ T.unpack oldtoken ++ "/" ++ T.unpack token ++ "/", cache]
          return $ LoginSession ctx $ BugzillaToken token
        NoToken -> do
          token <- bzLogin
          T.writeFile cache $ "[" <> brc <> "]\ntoken = " <> token <> "\n"
          putStrLn $ "Saved in " ++ cache
          return $ LoginSession ctx $ BugzillaToken token
      where
        rcParser :: IniParser BzTokenConf
        rcParser =
          section brc $
          BzTokenConf <$> fieldOf "token" string

        bzLogin :: IO T.Text
        bzLogin = do
          putStrLn "No valid bugzilla login token, please login:"
          passwd <- withoutEcho $ prompt "Bugzilla Password"
          if null passwd then bzLogin
            else do
            let anonsession = AnonymousSession ctx
                tokenReq = setRequestCheckStatus $
                           newBzRequest anonsession ["login"]
                           [("login", Just user),
                            makeTextItem "password" passwd]
            res <- getResponseBody <$> httpJSON tokenReq
            case lookupKey "token" res of
              Nothing -> T.putStrLn (lookupKey' "message" res) >> bzLogin
              Just token -> return token

        withoutEcho :: IO a -> IO a
        withoutEcho action =
          finally (hSetEcho stdin False >> action) (hSetEcho stdin True)

newtype BzTokenConf = BzTokenConf {bzToken :: T.Text}
  deriving (Eq, Show)

data BzTokenStatus = ValidToken BugzillaSession | InvalidToken T.Text | NoToken

bugIdIs :: BugId -> SearchExpression
bugIdIs bid = BugIdField .==. bid

reporterIs :: T.Text -> SearchExpression
reporterIs = (ReporterField .==.)

assigneeIs :: T.Text -> SearchExpression
assigneeIs = (AssignedToField .==.)

packageReview :: SearchExpression
packageReview =
  ComponentField .==. ["Package Review"]

statusOpen :: SearchExpression
statusOpen =
  StatusField ./=. "CLOSED"

statusNewAssigned :: SearchExpression
statusNewAssigned =
  StatusField `equalsAny` ["NEW", "ASSIGNED"]

statusNewPost :: SearchExpression
statusNewPost =
  StatusField `equalsAny` ["NEW", "ASSIGNED", "POST"]

statusNewModified :: SearchExpression
statusNewModified =
  StatusField `equalsAny` ["NEW", "ASSIGNED", "POST", "MODIFIED"]

reviewApproved :: SearchExpression
reviewApproved =
  FlagsField `contains` "fedora-review+"

pkgReviews :: String -> SearchExpression
pkgReviews pkg =
  SummaryField `contains` T.pack ("Review Request: " ++ pkg ++ " - ") .&&.
  packageReview

pkgBugs :: String -> SearchExpression
pkgBugs pkg =
  ComponentField .==. [T.pack pkg]

summaryContains :: String -> SearchExpression
summaryContains keywrd =
  SummaryField `contains` T.pack keywrd

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

approvedReviewBugSession :: String -> IO (Bug,BugzillaSession)
approvedReviewBugSession pkg = do
  (bugs,session) <- bugsSession $
                    pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bugs of
    [] -> error $ "No review bug found for " ++ pkg
    [bug] -> return (bug, session)
    _ -> error' "more than one review bug found!"

reviewBugToPackage :: Bug -> String
reviewBugToPackage =
  head . dropOne ["Request:"] . dropOne ["Review", "Re-Review", "Rename"]  . words . T.unpack . bugSummary
  where
    dropOne _ [] = []
    dropOne ks as@(w:ws) = if w `elem` ks then ws else as

readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
readIniConfig inifile iniparser fn = do
  ini <- T.readFile inifile
  return $ either error fn $ parseIniFile ini iniparser

sortBugsByStatus :: [Bug] -> [Bug]
sortBugsByStatus = sortOn (bugStatusEnum . bugStatus)

sortBugsByProduct :: [Bug] -> [Bug]
sortBugsByProduct = sortOn bugProduct

-- FIXME make datatype
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
  T.putStrLn $ "(Comment " <> intAsText (commentCount cmt) <> ") <" <> commentCreator cmt <> "> " <> (T.pack . show) (commentCreationTime cmt) <> "\n"
  mapM_ (T.putStrLn . ("  " <>)) $ dropDuplicates . removeLeadingNewline . T.lines $ commentText cmt
  putStrLn ""

checkRepoCreatedComment :: BugzillaSession -> BugId -> IO Bool
checkRepoCreatedComment session bid =
    checkForComment session bid
      "(fedscm-admin):  The Pagure repository was created at"

checkForComment :: BugzillaSession -> BugId -> T.Text -> IO Bool
checkForComment session bid text = do
    comments <- map commentText <$> getComments session bid
    return $ any (text `T.isInfixOf`) $ reverse comments

putReviewBug :: Bool -> Bug -> IO ()
putReviewBug short bug = do
  if short then putStr $ reviewBugToPackage bug ++ " "
    else do
    putStrLn $ reviewBugToPackage bug ++ " (" ++ T.unpack (bugStatus bug) ++ ")"
    putBugId $ bugId bug
    putStrLn ""

putBug :: Bug -> IO ()
putBug bug = do
  T.putStrLn $ bugSummary bug <> " (" <> bugStatus bug <> ")"
  putBugId $ bugId bug
  putStrLn ""

putBugVer :: Bug -> IO ()
putBugVer bug = do
  T.putStr $ "[" <> prodVersion <> "] "
  putBug bug
  where
    prodVersion = T.unwords (bugVersion bug)

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

-- testBZlogin :: IO ()
-- testBZlogin =
--   void bzLoginSession

-- | make a key-value
makeTextItem :: String -> String -> (T.Text, Maybe T.Text)
makeTextItem k val = (T.pack k, Just (T.pack val))

listBzUsers :: BugzillaSession -> String -> IO [Object]
listBzUsers session user = do
  let req = setRequestCheckStatus $
            newBzRequest session ["user"] [makeTextItem "match" user]
  lookupKey' "users" . getResponseBody <$> httpJSON req
