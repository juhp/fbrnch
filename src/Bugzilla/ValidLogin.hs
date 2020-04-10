{-# LANGUAGE OverloadedStrings   #-}

module Bugzilla.ValidLogin
  (ValidLogin(..))
where

import Control.Monad (mzero)
import Data.Aeson(Value(..), FromJSON(..), ToJSON(..),
                   pairs,
                   (.:), (.=), object)

newtype ValidLogin = ValidLogin {
    validToken :: Bool
  } deriving (Show,Eq)

instance FromJSON ValidLogin where
  parseJSON (Object v) = ValidLogin <$> v .:  "result"
  parseJSON _          = mzero


instance ToJSON ValidLogin where
  toJSON     (ValidLogin result) = object ["result" .= result]
  toEncoding (ValidLogin result) = pairs  ("result" .= result)
