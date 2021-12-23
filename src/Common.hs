{-# LANGUAGE CPP #-}

module Common (
  module Control.Monad.Extra,
  module Data.List.Extra,
  module Data.Maybe,
#if !MIN_VERSION_base(4,11,0)
  (<>),
#endif
  (+/+),
  plural
  ) where

import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Network.HTTP.Query ((+/+))

plural :: Int -> String -> String
plural i ns =
  mconcat
  [
    show i,
    " ",
    ns,
    if i == 1 then "" else "s"
  ]
