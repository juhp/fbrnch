{-# LANGUAGE OverloadedStrings #-}

module Bodhi (
  bodhiCreateOverride,
  checkAutoBodhiUpdate,
  UpdateType(..)
  )
where

import Data.Aeson.Types (Object, (.:), parseEither)
import Data.Char (toLower)
import Fedora.Bodhi hiding (bodhiUpdate)
import Text.Read
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP


import Branches
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
        either errMsg id $ parseEither (.: k) obj

-- FIXME should determine 3 days for branched devel release
-- FIXME handle expired override?
bodhiCreateOverride :: String -> IO ()
bodhiCreateOverride nvr = do
  putStrLn $ "Creating Bodhi Override for " ++ nvr ++ ":"
  ok <- cmdBool "bodhi" ["overrides", "save", "--notes", "chain building with fbrnch", "--duration", "4", nvr]
  if ok
    then putStrLn $ "https://bodhi.fedoraproject.org/overrides/" ++ nvr
    else do
    moverride <- bodhiOverride nvr
    case moverride of
      Nothing -> do
        putStrLn "bodhi override failed"
        prompt_ "Press Enter to retry"
        bodhiCreateOverride nvr
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
