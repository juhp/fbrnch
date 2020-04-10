{-# LANGUAGE CPP #-}

module Common.Text (
  module Data.Text,
  module Data.Text.IO

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
, (<>)
#endif
  ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Data.Text
import Data.Text.IO
