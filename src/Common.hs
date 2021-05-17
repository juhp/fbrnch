{-# LANGUAGE CPP #-}

module Common (
  module Control.Monad.Extra,
  module Data.List.Extra,
  module Data.Maybe,
#if !MIN_VERSION_base(4,11,0)
  (<>),
#endif
  (+/+)
  ) where

import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Network.HTTP.Query ((+/+))
