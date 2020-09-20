{-# LANGUAGE OverloadedStrings #-}

module Bodhi (
  checkAutoBodhiUpdate,
  bodhiCreateOverride)
where

import Data.Aeson.Types (Object, (.:), parseEither)
import Fedora.Bodhi hiding (bodhiUpdate)

import Branches
import Common
import Common.System
import qualified Common.Text as T
import Prompt

checkAutoBodhiUpdate :: Branch -> IO Bool
checkAutoBodhiUpdate Master = return True
checkAutoBodhiUpdate br =
  lookupKey'' "create_automatic_updates" <$> bodhiRelease (show br)
  where
    -- Error in $: key "create_automatic_updates" not found
    lookupKey'' :: T.Text -> Object -> Bool
    lookupKey'' k obj =
      let errMsg e = error $ e ++ " " ++ show obj in
        either errMsg id $ parseEither (.: k) obj

bodhiCreateOverride :: String -> IO ()
bodhiCreateOverride nvr = do
  putStrLn $ "Creating Bodhi Override for " ++ nvr ++ ":"
  ok <- cmdBool "bodhi" ["overrides", "save", "--notes", "chain building with fbrnch", "--duration", "7", nvr]
  unless ok $ do
    moverride <- bodhiOverride nvr
    case moverride of
      Nothing -> do
        putStrLn "bodhi override failed"
        prompt_ "Press Enter to retry"
        bodhiCreateOverride nvr
      Just obj -> print obj
