{-# LANGUAGE OverloadedStrings   #-}

module Bugzilla.Login
  (Login(..))
where

import Data.Aeson((.:), FromJSON(..), withObject)

import qualified Common.Text as T

data Login = Login {
    loginId :: Int,
    loginToken :: T.Text
  } deriving (Show)

instance FromJSON Login where
    parseJSON = withObject "Login" $ \v -> Login
        <$> v .: "id"
        <*> v .: "token"
