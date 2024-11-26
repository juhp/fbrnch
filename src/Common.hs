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
  indefinite,
  plural,
  pluralException,
  singularVerb,
  putNewLn,
  reverseSort,
  showNVR
  ) where

import Control.Monad.Extra -- hiding (loop)
import Data.List.Extra hiding (list, merge,
#if MIN_VERSION_extra(1,6,19)
                               headDef
#endif
                              )
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
  pluralException i Nothing ns (ns ++ "s")

pluralException :: Int -> Maybe String -> String -> String -> String
pluralException 0 (Just z) _ _ = z
pluralException i _ ns ps =
  mconcat
  [
    if i == 0 then "no" else show i,
    " ",
    if i == 1 then ns else ps
  ]

singularVerb :: Bool -> String -> String
singularVerb singular v = v ++ if singular then "s" else ""

indefinite :: String -> String
indefinite "" = ""
indefinite w@(c:_) =
  (if c `elem` "aeiou" then "an" else "a") +-+ w

putNewLn :: IO ()
putNewLn = putChar '\n'

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy (comparing Down)
