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
  error',
#if !MIN_VERSION_simple_cmd(0,2,5)
  timeIO,
#endif
  timeIODesc
  ) where

import Control.Monad.IO.Class
#if !MIN_VERSION_filepath(1,4,2)
import Data.List
#endif
#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_simple_cmd(0,2,8)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
#if !MIN_VERSION_simple_cmd(0,2,8)
import Safe
#endif
import SimpleCmd hiding (
  error',
#if !MIN_VERSION_simple_cmd(0,2,8)
  cmdFull,
#endif
#if MIN_VERSION_simple_cmd(0,2,1)
  ifM,whenM
#endif
  )
import System.Exit
#if !MIN_VERSION_simple_cmd(0,2,8)
import System.Process
#endif
import System.Directory
import System.FilePath
import System.IO

#if !MIN_VERSION_simple_cmd(0,2,8)
import Control.Exception (bracket)
import Data.Time.Clock
#endif

#if !MIN_VERSION_simple_cmd(0,2,5)
timeIO :: IO a -> IO a
timeIO = timeIOHelper "took"
#endif

timeIODesc :: String -> IO a -> IO a
timeIODesc thing = timeIOHelper (thing +-+ "took")

#if !MIN_VERSION_simple_cmd(0,2,8)
timeIOHelper :: String -> IO a -> IO a
timeIOHelper msg action = do
  bracket
    getCurrentTime
    (\start -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ msg +-+ renderDuration duration)
    (const action)
  where
#if MIN_VERSION_time(1,9,0)
    renderDuration dur =
      let fmtstr
            | dur < 60 = "%s sec"
            | dur < 3600 = "%m min %S sec"
            | dur < 7200 = "%h hour %M min"
            | otherwise = "%h hours %M min"
      in formatTime defaultTimeLocale fmtstr dur
#else
    renderDuration = show
#endif
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

error':: MonadIO m => String -> m a
error' err = do
  liftIO $ do
    putStrLn err
    exitFailure
