{-# LANGUAGE CPP #-}

module Common.System (
  module SimpleCmd,
  module System.Directory,
  module System.FilePath,
  getDirectoryName,
  isTty,
  setNoBuffering,
#if !MIN_VERSION_simple_cmd(0,2,3)
  cmdFull
#endif
  ) where

import SimpleCmd
#if MIN_VERSION_simple_cmd(0,2,1)
  hiding (ifM,whenM)
#endif
#if !MIN_VERSION_simple_cmd(0,2,3)
import System.Exit
import System.Process
#endif
import System.Directory
import System.FilePath
import System.IO

isTty :: IO Bool
isTty = hIsTerminalDevice stdin

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

getDirectoryName :: IO String
getDirectoryName =
  takeFileName <$> getCurrentDirectory

#if !MIN_VERSION_simple_cmd(0,2,3)
cmdFull :: String -> [String] -> String -> IO (Bool, String, String)
cmdFull c args input = do
  (ret, out, err) <- readProcessWithExitCode c args input
  return (ret == ExitSuccess, removeTrailingNewline out, removeTrailingNewline err)
  where
    removeTrailingNewline :: String -> String
    removeTrailingNewline "" = ""
    removeTrailingNewline str =
      if last str == '\n'
      then init str
      else str
#endif
