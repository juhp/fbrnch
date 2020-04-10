{-# LANGUAGE CPP #-}

module Common (
  module Control.Monad,
  module Data.List,
  module Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
, (<>)
#endif
  ) where

import Control.Monad
import Data.List
import Data.Maybe

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
