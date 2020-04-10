{-# LANGUAGE OverloadedStrings   #-}

module Bugzilla.NewId
  (NewId(..))
where

import           Control.Monad      (mzero)
import           Data.Aeson(Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.=), object)

newtype NewId = NewId {
    newId :: Int
  } deriving (Show,Eq)


instance FromJSON NewId where
  parseJSON (Object v) = NewId <$> v .:  "id"
  parseJSON _          = mzero


instance ToJSON NewId where
  toJSON     (NewId i) = object ["id" .= i]
  toEncoding (NewId i) = pairs  ("id" .= i)
