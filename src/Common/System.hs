{-# LANGUAGE CPP #-}

module Common.System (
  module SimpleCmd,
  module System.Directory,
  module System.FilePath,
  isTty,
  setNoBuffering
  ) where

import SimpleCmd
#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,1))
  hiding (ifM,whenM)
#endif
import System.Directory
import System.FilePath
import System.IO

isTty :: IO Bool
isTty = hIsTerminalDevice stdin

setNoBuffering :: IO ()
setNoBuffering =
  hSetBuffering stdout NoBuffering
