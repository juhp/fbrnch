{-# LANGUAGE CPP #-}

module Common.System (
  module SimpleCmd,
  module System.Directory,
  module System.FilePath,
  getDirectoryName,
  isTty,
  setNoBuffering,
#if !MIN_VERSION_filepath(1,4,2)
  isExtensionOf,
#endif
#if !MIN_VERSION_simple_cmd(0,2,8)
  cmdFull,
#endif
#if !MIN_VERSION_simple_cmd(0,2,5)
  timeIO,
#endif
  ) where

#if !MIN_VERSION_filepath(1,4,2)
import Data.List
#endif
import Safe
import SimpleCmd hiding (
#if !MIN_VERSION_simple_cmd(0,2,8)
  cmdFull,
#endif
#if MIN_VERSION_simple_cmd(0,2,1)
  ifM,whenM
#endif
  )
#if !MIN_VERSION_simple_cmd(0,2,8)
import System.Exit
import System.Process
#endif
import System.Directory
import System.FilePath
import System.IO

#if !MIN_VERSION_simple_cmd(0,2,5)
import Control.Exception
import Data.Time.Clock

timeIO :: IO a -> IO a
timeIO action = do
  bracket
    getCurrentTime
    (\start -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "took" +-+ show duration)
    (const action)
#endif

isTty :: IO Bool
isTty = hIsTerminalDevice stdin

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

getDirectoryName :: IO String
getDirectoryName =
  takeFileName <$> getCurrentDirectory

-- bugfix for lazy stderr
#if !MIN_VERSION_simple_cmd(0,2,8)
cmdFull :: String -> [String] -> String -> IO (Bool, String, String)
cmdFull c args input = do
  (ret, out, err) <- readProcessWithExitCode c args input
  return (ret == ExitSuccess, removeTrailingNewline out, removeTrailingNewline err)
  where
    removeTrailingNewline :: String -> String
    removeTrailingNewline str =
      if lastMay str == Just '\n'
      then init str
      else str
#endif

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
