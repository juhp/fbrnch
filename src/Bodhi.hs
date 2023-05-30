{-# LANGUAGE CPP, OverloadedStrings #-}

module Bodhi (
  bodhiCreateOverride,
  bodhiTestingRepo,
  checkAutoBodhiUpdate,
  UpdateType(..),
  UpdateSeverity(..),
  bodhiUpdate,
  bodhiBuildExists
  )
where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types (Object, (.:), parseEither)
import Data.Char (isDigit)
import Fedora.Bodhi hiding (bodhiUpdate)
import SimplePrompt (promptEnter, promptNonEmpty)
import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP

import Branches
import Bugzilla (BugId)
import Common
import Common.System
import qualified Common.Text as T
import Package

checkAutoBodhiUpdate :: Branch -> IO Bool
checkAutoBodhiUpdate Rawhide = return True
-- epel7 returns 'create_automatic_updates: null' !
checkAutoBodhiUpdate (EPEL 7) = return False
-- not sure how to best handle next
checkAutoBodhiUpdate (EPELNext _) = return False
checkAutoBodhiUpdate br =
  lookupKey'' "create_automatic_updates" <$> bodhiRelease (show br)
  where
    -- Error in $: key "create_automatic_updates" not found
    lookupKey'' :: T.Text -> Object -> Bool
    lookupKey'' k obj =
      let errMsg e = error $ e +-+ show obj in
        -- bodhi-hs has lookupKeyEither
        either errMsg id $ parseEither (.: fromText k) obj

#if !MIN_VERSION_aeson(2,0,0)
    fromText :: T.Text -> T.Text
    fromText = id
#endif

-- FIXME should determine 3 days for branched devel release
-- FIXME handle expired override?
bodhiCreateOverride :: Bool -> Maybe Int -> String -> IO ()
bodhiCreateOverride dryrun mduration nvr = do
  putStrLn $ "Creating Bodhi Override for" +-+ nvr ++ ":"
  unless dryrun $ do
    ok <- cmdBool "bodhi" ["overrides", "save", "--notes", "chain building with fbrnch", "--duration", show (fromMaybe 4 mduration), "--no-wait", nvr]
    if ok
      then putStrLn $ "https://bodhi.fedoraproject.org/overrides/" ++ nvr
      else do
      moverride <- bodhiOverride nvr
      case moverride of
        Nothing -> do
          putStrLn "bodhi override failed"
          promptEnter "Press Enter to retry"
          bodhiCreateOverride dryrun mduration nvr
        -- FIXME prettyprint
        Just obj -> error' $ show obj

data UpdateType =
  SecurityUpdate | BugfixUpdate | EnhancementUpdate | NewPackageUpdate |
  TemplateUpdate
  deriving Eq

instance Show UpdateType where
  show SecurityUpdate = "security"
  show BugfixUpdate = "bugfix"
  show EnhancementUpdate = "enhancement"
  show NewPackageUpdate = "newpackage"
  show TemplateUpdate = error "template update"

instance Read UpdateType where
  readPrec = do
    s <- look
    case lower s of
      "security" -> RP.lift (R.string s) >> return SecurityUpdate
      "bugfix" -> RP.lift (R.string s) >> return BugfixUpdate
      "enhancement" -> RP.lift (R.string s) >> return EnhancementUpdate
      "newpackage" -> RP.lift (R.string s) >> return NewPackageUpdate
      "template" -> RP.lift (R.string s) >> return TemplateUpdate
      _ -> error' "unknown bodhi update type" >> RP.pfail

data UpdateSeverity =
  SeverityLow | SeverityMedium | SeverityHigh | SeverityUrgent |
  SeverityUnspecified
  deriving Eq

instance Show UpdateSeverity where
  show SeverityLow = "low"
  show SeverityMedium = "medium"
  show SeverityHigh = "high"
  show SeverityUrgent = "urgent"
  show SeverityUnspecified = "unspecified"

instance Read UpdateSeverity where
  readPrec = do
    s <- look
    case lower s of
      "low" -> RP.lift (R.string s) >> return SeverityLow
      "medium" -> RP.lift (R.string s) >> return SeverityMedium
      "high" -> RP.lift (R.string s) >> return SeverityHigh
      "urgent" -> RP.lift (R.string s) >> return SeverityUrgent
      _ -> error' "unknown bodhi update severity" >> RP.pfail

bodhiTestingRepo :: Branch -> IO (Maybe String)
bodhiTestingRepo Rawhide = return Nothing
bodhiTestingRepo br = do
  obj <- bodhiRelease (show br)
  return $
    case lookupKey "testing_repository" obj :: Maybe String of
      Nothing -> Nothing
      Just _ -> lookupKey' "testing_tag" obj

-- push comma separated list of builds for a package to bodhi
bodhiUpdate :: Bool -> (Maybe UpdateType, UpdateSeverity) -> Maybe BugId
            -> Bool -> FilePath -> String -> IO ()
bodhiUpdate _ _ _ _ _ [] = error' "cannot make empty update"
bodhiUpdate dryrun (mupdate,severity) mreview usechangelog spec nvrs = do
  case mupdate of
    Nothing -> return ()
    Just updateType ->
      unless dryrun $ do
        -- use cmdLog to debug, but notes are not quoted
        updatedone <- do
          mtemplate <- maybeTemplate updateType
          case mtemplate of
            Just file -> do
              cmd_ "bodhi" ["updates", "new", "--file", file, nvrs]
              return True
            Nothing -> do
              -- FIXME also query for open existing bugs
              changelog <- if isJust mreview
                           then getSummaryURL spec
                           else
                             if usechangelog
                             then cleanChangelog spec
                             else
                               -- FIXME list open bugs
                               changeLogPrompt True spec
              if trim (lower changelog) `elem` ["no","n"]
                then return False
                else do
                let cbugs = extractBugReferences changelog
                    bugs =
                      let bids = [show rev | Just rev <- [mreview]] ++ cbugs in
                        if null bids
                        then []
                        else ["--bugs", intercalate "," bids]
                when (isJust mreview &&
                      updateType `elem` [SecurityUpdate,BugfixUpdate]) $
                  warning "overriding update type with 'newpackage'"
                putStrLn $ "Creating Bodhi Update for" +-+ nvrs ++ ":"
                -- FIXME check for Bodhi URL to confirm update
                -- FIXME returns json error string if it exists:
                -- {"status": "error", "errors": [{"location": "body", "name": "builds", "description": "Update for ghc9.2-9.2.5-14.fc36 already exists"}]}
                cmd_ "bodhi" $ ["updates", "new", "--type", if isJust mreview then "newpackage" else show updateType, "--severity", show severity, "--request", "testing", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvrs]
                return True
        when updatedone $ do
          -- FIXME avoid this if we know the update URLs (split update does not seem to return URLs)
          updates <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" nvrs]
          if null updates
            then do
            putStrLn $ "bodhi submission failed for" +-+ nvrs
            promptEnter "Press Enter to resubmit to Bodhi"
            bodhiUpdate dryrun (mupdate,severity) mreview usechangelog spec nvrs
            else
            forM_ updates $ \update ->
            case lookupKey "url" update of
              Nothing -> error' "Update created but no url"
              Just uri -> putStrLn uri
  where
    extractBugReferences :: String -> [String]
    extractBugReferences clog =
      case dropWhile (/= '#') clog of
        "" -> []
        rest ->
          case span isDigit (tail rest) of
            (ds,more) ->
              -- make sure is contemporary 7-digit bug
              (if length ds > 6 then (ds :) else id) $
              extractBugReferences more

    maybeTemplate :: UpdateType -> IO (Maybe FilePath)
    maybeTemplate TemplateUpdate = do
      file <- promptNonEmpty "Please input the update template filepath"
      exists <- doesFileExist file
      if exists
        then return $ Just file
        else do
        putStrLn ("no such file:" +-+ file)
        maybeTemplate TemplateUpdate
    maybeTemplate _ = return Nothing

bodhiBuildExists :: String -> IO Bool
bodhiBuildExists nvr = do
  obj <- bodhiBuild nvr
  return $ isNothing (lookupKey "status" obj :: Maybe String)
