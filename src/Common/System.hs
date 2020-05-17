{-# LANGUAGE CPP #-}

module Common.System (
  module SimpleCmd,
  module System.Directory,
  module System.FilePath
  ) where

import SimpleCmd
#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,1))
  hiding (ifM)
#endif
import System.Directory
import System.FilePath
