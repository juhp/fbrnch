{-# LANGUAGE OverloadedStrings #-}

module Bugzilla (
  Bug(..),
  User(..),
  BugId,
  -- session
  BugzillaSession,
  bugsSession,
  bugsAnon,
  bugIdsAnon,
  bzAnonSession,
  bzApiKeySession,
  bzReviewSession,
  bzReviewAnon,
  bzBugMaybe,
  getBzUser,
  getBzAccountId,
  reviewBugIdSession,
  approvedReviewBugIdSession,
  approvedReviewBugSession,
  pkgBugs,
  pkgReviews,
  pkgReviewsPrefix,
  -- listBzUsers,
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
  versionIs,
  ftbfsFedoraBugs,
  componentSubStr,
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
  sortBugsByID,
  sortBugsByProduct,
  sortBugsByStatus,
  -- output
  putBug,
  putBugVer,
  putReviewBug,
  putBugId,
  putBugURLStatus,
  -- request
  newBzRequest,
  makeTextItem,
  searchUsers
  ) where

import Common
import Common.System
import qualified Common.Text as T
import SimplePrompt

import Data.Aeson.Types (Array, Object)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8
import Data.Ini.Config
import Network.HTTP.Conduit
import Network.HTTP.Query
import Network.HTTP.Simple
import System.Environment.XDG.BaseDir
import qualified Text.Email.Validate as Email
import Web.RedHatBugzilla
import Web.RedHatBugzilla.Search

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
    error' $ "failed to update bug" +-+ show bid
  putStrLn "Comment added"

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
    error' $ "failed to update bug" +-+ show bid

encodeParams :: [(String, String)] -> [(ByteString, ByteString)]
encodeParams [] = []
encodeParams ((k,v):ps) =
  (B.pack k, fromString v) : encodeParams ps

-- FIXME check original status?
putBugBuild :: Bool -> BugzillaSession -> BugId -> String -> IO ()
putBugBuild dryrun session bid nvr = do
  unless dryrun $
    void $ updateBug session bid
    [("cf_fixed_in", nvr), ("status", "MODIFIED")]
  putStrLn $ "bug" +-+ show bid ++ (if dryrun then " would be" else "") +-+ "moved to MODIFIED with" +-+ nvr

brc :: T.Text
brc = "bugzilla.redhat.com"

bzBugMaybe :: BugzillaSession -> SearchExpression -> IO (Maybe Bug)
bzBugMaybe session query = do
  bugs <- searchBugs session query
  return $ case bugs of
             [] -> Nothing
             [bug] -> Just bug
             _ -> error' "more that one bug found"

bzReviewAnon :: IO (Maybe BugId)
bzReviewAnon = do
  pkg <- getDirectoryName
  bids <- bugIdsAnon $
          pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  return $ case bids of
    [bid] -> Just bid
    _ -> Nothing

bzReviewSession :: IO (Maybe (BugId,BugzillaSession))
bzReviewSession = do
  pkg <- getDirectoryName
  bids <- bugIdsAnon $
          pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bids of
    [bid] -> do
      session <- bzApiKeySession
      return $ Just (bid, session)
    _ -> return Nothing

newtype BzUserRC = BzUserRC {rcUserEmail :: UserEmail}
  deriving (Eq, Show)

emailIsValid :: String -> Bool
emailIsValid = Email.isValid . B.pack

getBzUser :: IO UserEmail
getBzUser = do
  config <- getUserConfigFile "fbrnch" "bugzilla"
  haveConfig <- doesFileExist config
  if haveConfig
    then readIniConfig config rcParser rcUserEmail
    else do
    -- FIXME: option to override email
    email <- prompt "Bugzilla Username"
    when (emailIsValid email) $ do
      let configDir = takeDirectory config
      configDirExists <- doesDirectoryExist configDir
      unless configDirExists $ createDirectory configDir
      T.writeFile config $ "[" <> brc <> "]\nuser = " <> T.pack email <> "\n"
      putStrLn $ "Saved in" +-+ config ++ "\n"
    getBzUser
  where
    rcParser :: IniParser BzUserRC
    rcParser =
      section brc $
      BzUserRC <$> fieldOf "user" string

bzAnonSession :: BugzillaSession
bzAnonSession = anonymousSession brc

-- bzAnonSession' :: IO (BugzillaSession,BugzillaContext)
-- bzAnonSession' =
--   context <- newBugzillaContext brc
--   return (AnonymousSession context, context)

bzApiKeySession :: IO BugzillaSession
bzApiKeySession = do
  (config,exists) <- do
    rc1 <- getUserConfigFile "python-bugzilla" "bugzillarc"
    haveRc1 <- doesFileExist rc1
    if haveRc1
      then return (rc1,haveRc1)
      else do
      home <- getHomeDirectory
      let rc2 = home </> ".bugzillarc"
      haveRc2 <- doesFileExist rc2
      return $ if haveRc2
               then (rc2,haveRc2)
               else (rc1,False)
  if not exists
    then
    -- do
    -- let configDir = takeDirectory config
    -- configDirExists <- doesDirectoryExist configDir
    -- unless configDirExists $ createDirectory configDir
    error' $ unlines
    ["No Bugzilla API key found",
     "Create a key at https://bugzilla.redhat.com/userprefs.cgi?tab=apikey",
     "Save the key under in '" ++ config ++ "' under:",
     "[bugzilla.redhat.com]",
     apiKeyField +-+ "= <YourApiKey>"]
    else do
    apikey <- readIniConfig config rcParser bzApiKey
    return $ ApiKeySession brc $ BugzillaApiKey apikey
  where
    apiKeyField = "api_key"

    rcParser :: IniParser BzApiKeyConf
    rcParser =
      section brc $
      BzApiKeyConf <$> fieldOf (T.pack apiKeyField) string

newtype BzApiKeyConf = BzApiKeyConf {bzApiKey :: T.Text}
  deriving (Eq, Show)

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
  SummaryField `contains` T.pack ("Review Request:" +-+ pkg +-+ "- ") .&&.
  packageReview

pkgReviewsPrefix :: String -> SearchExpression
pkgReviewsPrefix prefix =
  SummaryField `contains` T.pack ("Review Request:" +-+ prefix) .&&.
  packageReview

pkgBugs :: String -> SearchExpression
pkgBugs pkg =
  ComponentField .==. [T.pack pkg]

summaryContains :: String -> SearchExpression
summaryContains keywrd =
  SummaryField `contains` T.pack keywrd

versionIs :: String -> SearchExpression
versionIs v =
  VersionField .==. T.pack v

ftbfsFedoraBugs :: SearchExpression
ftbfsFedoraBugs = summaryContains "FTBFS in Fedora"

componentSubStr :: String -> SearchExpression
componentSubStr substr =
  ComponentField .=~. [T.pack substr]

bugIdsAnon :: SearchExpression -> IO [BugId]
bugIdsAnon = searchBugs' bzAnonSession

bugsAnon :: SearchExpression -> IO [Bug]
bugsAnon = searchBugs bzAnonSession

-- FIXME name is ambiguous
bugsSession :: SearchExpression -> IO ([Bug],BugzillaSession)
bugsSession query = do
  session <- bzApiKeySession
  bugs <- searchBugs session query
  return (bugs, session)

reviewBugIdSession :: String -> IO (BugId,BugzillaSession)
reviewBugIdSession pkg = do
  bugs <- bugIdsAnon $ pkgReviews pkg .&&. statusOpen
  case bugs of
    [] -> error $ "No review bug found for" +-+ pkg
    [bug] -> do
      session <- bzApiKeySession
      return (bug, session)
    _ -> error' "more than one review bug found!"

approvedReviewBugIdSession :: String -> IO (BugId,BugzillaSession)
approvedReviewBugIdSession pkg = do
  bugs <- bugIdsAnon $
          pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bugs of
    [] -> error $ "No review bug found for" +-+ pkg
    [bug] -> do
      session <- bzApiKeySession
      return (bug, session)
    _ -> error' "more than one review bug found!"

approvedReviewBugSession :: String -> IO (Bug,BugzillaSession)
approvedReviewBugSession pkg = do
  bugs <- bugsAnon $
          pkgReviews pkg .&&. statusOpen .&&. reviewApproved
  case bugs of
    [] -> error $ "No review bug found for" +-+ pkg
    [bug] -> do
      session <- bzApiKeySession
      return (bug, session)
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
  return $
    case parseIniFile ini iniparser of
      Left err -> error' $ err ++ "\nin" +-+ inifile
      Right res -> fn res

sortBugsByID :: [Bug] -> [Bug]
sortBugsByID = sortOn bugId

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
  putNewLn

-- FIXME check for Fedora Admin user for bugzilla script actions <fedora-admin-xmlrpc@fedoraproject.org>
checkRepoCreatedComment :: BugzillaSession -> BugId -> IO Bool
checkRepoCreatedComment session bid =
    checkForComment session bid
      "The Pagure repository was created at https://src.fedoraproject.org/"

checkForComment :: BugzillaSession -> BugId -> T.Text -> IO Bool
checkForComment session bid text = do
    comments <- map commentText <$> getComments session bid
    return $ any (text `T.isInfixOf`) $ reverse comments

putReviewBug :: Bool -> Bug -> IO ()
putReviewBug short bug = do
  if short
    then putStr $ reviewBugToPackage bug ++ " "
    else do
    putStr $ T.unpack (bugStatus bug) +-+ reviewBugToPackage bug ++ ": "
    putBugId $ bugId bug

putBug :: Bug -> IO ()
putBug bug = do
  T.putStrLn $ bugSummary bug <> " (" <> bugStatus bug <> ")"
  putBugId $ bugId bug
  putNewLn

putBugVer :: Bug -> IO ()
putBugVer bug = do
  T.putStr $ "[" <> prodVersion <> "] "
  putBug bug
  where
    prodVersion = T.unwords (bugVersion bug)

putBugId :: BugId -> IO ()
putBugId bid =
  putStrLn $ "https://" <> T.unpack brc <> "/show_bug.cgi?id=" <> show bid

putBugURLStatus :: Bug -> IO ()
putBugURLStatus bug = do
  putStr $ "https://" <> T.unpack brc <> "/show_bug.cgi?id=" <> show (bugId bug)
  T.putStrLn $ " (" <> bugStatus bug <> ")"

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

getBzAccountId :: BugzillaSession -> Maybe String -> IO T.Text
getBzAccountId session muser = do
  case muser of
    Nothing -> getBzUser
    Just userid ->
      if emailIsValid userid then return $ T.pack userid
      else do
        users <- listBzUsers session userid
        case users of
          [] -> error' $ "No user found for" +-+ userid
          [obj] -> return $ T.pack $ lookupKey' "email" obj
          objs -> error' $ "Found multiple user matches:" +-+
                  unwords (map (lookupKey' "email") objs)
