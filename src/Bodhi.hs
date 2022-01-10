{-# LANGUAGE CPP, OverloadedStrings #-}

module Bodhi (
  bodhiCreateOverride,
  bodhiTestingRepo,
  checkAutoBodhiUpdate,
  UpdateType(..)
  )
where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif
import Data.Aeson.Types (Object, (.:), parseEither)
import Data.Char (toLower)
import Fedora.Bodhi hiding (bodhiUpdate)
import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP


import Branches
import Common
import Common.System
import qualified Common.Text as T
import Prompt

checkAutoBodhiUpdate :: Branch -> IO Bool
checkAutoBodhiUpdate Rawhide = return True
-- epel7 returns 'create_automatic_updates: null' !
checkAutoBodhiUpdate (EPEL 7) = return False
checkAutoBodhiUpdate br =
  lookupKey'' "create_automatic_updates" <$> bodhiRelease (show br)
  where
    -- Error in $: key "create_automatic_updates" not found
    lookupKey'' :: T.Text -> Object -> Bool
    lookupKey'' k obj =
      let errMsg e = error $ e ++ " " ++ show obj in
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
  putStrLn $ "Creating Bodhi Override for " ++ nvr ++ ":"
  unless dryrun $ do
    ok <- cmdBool "bodhi" ["overrides", "save", "--notes", "chain building with fbrnch", "--duration", show (fromMaybe 4 mduration), "--no-wait", nvr]
    if ok
      then putStrLn $ "https://bodhi.fedoraproject.org/overrides/" ++ nvr
      else do
      moverride <- bodhiOverride nvr
      case moverride of
        Nothing -> do
          putStrLn "bodhi override failed"
          prompt_ "Press Enter to retry"
          bodhiCreateOverride dryrun mduration nvr
        -- FIXME prettyprint
        Just obj -> error' $ show obj

data UpdateType =
  SecurityUpdate | BugfixUpdate | EnhancementUpdate | NewPackageUpdate

instance Show UpdateType where
  show SecurityUpdate = "security"
  show BugfixUpdate = "bugfix"
  show EnhancementUpdate = "enhancement"
  show NewPackageUpdate = "newpackage"

instance Read UpdateType where
  readPrec = do
    s <- look
    case map toLower s of
      "security" -> RP.lift (R.string s) >> return SecurityUpdate
      "bugfix" -> RP.lift (R.string s) >> return BugfixUpdate
      "enhancement" -> RP.lift (R.string s) >> return EnhancementUpdate
      "newpackage" -> RP.lift (R.string s) >> return NewPackageUpdate
      _ -> error' "unknown bodhi update type" >> RP.pfail

bodhiTestingRepo :: Branch -> IO (Maybe String)
bodhiTestingRepo Rawhide = return Nothing
bodhiTestingRepo br = do
  obj <- bodhiRelease (show br)
  return $ case lookupKey "testing_repository" obj :: Maybe String of
             Nothing -> Nothing
             Just _ -> lookupKey' "testing_tag" obj
