{-# LANGUAGE CPP #-}

module Common (
  module Control.Monad.Extra,
  module Data.List.Extra,
  module Data.Maybe,
#if !MIN_VERSION_base(4,11,0)
  (<>),
#endif
  (+/+),
  (+-+),
  plural,
  pluralException,
  putNewLn,
  reverseSort,
  showNVR
  ) where

import Control.Monad.Extra -- hiding (loop)
import Data.List.Extra hiding (list, merge)
import Data.Maybe
import Data.Ord (comparing, Down(Down))
import Data.RPM.NVR (showNVR)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Network.HTTP.Query ((+/+))
import SimpleCmd ((+-+))

plural :: Int -> String -> String
plural i ns =
  pluralException i ns (ns ++ "s")

pluralException :: Int -> String -> String -> String
pluralException i ns ps =
  mconcat
  [
    if i == 0 then "no" else show i,
    " ",
    if i == 1 then ns else ps
  ]

putNewLn :: IO ()
putNewLn = putChar '\n'

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy (comparing Down)
