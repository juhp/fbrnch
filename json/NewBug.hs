{-# LANGUAGE OverloadedStrings   #-}

module NewBug
  (NewBug(..))
where

import           Control.Monad      (mzero)
import           Data.Aeson(Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.=), object)

data NewBug = NewBug {
    newBugId :: Int
  } deriving (Show,Eq)


instance FromJSON NewBug where
  parseJSON (Object v) = NewBug <$> v .:  "id"
  parseJSON _          = mzero


instance ToJSON NewBug where
  toJSON     (NewBug bid) = object ["id" .= bid]
  toEncoding (NewBug bid) = pairs  ("id" .= bid)
