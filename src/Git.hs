{-# LANGUAGE CPP #-}

module Git (
  gitBool,
  gitPull,
  gitPushSilent,
  gitShortLog,
  gitShortLogN,
  gitShortLog1,
  simplifyCommitLog,
  checkWorkingDirClean,
  workingDirClean,
  module SimpleCmd.Git
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import SimpleCmd
import SimpleCmd.Git

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,2))
#else
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

gitPull :: IO ()
gitPull = do
  pull <- git "pull" ["--rebase"]
  unless ("Already up to date." `isPrefixOf` pull) $
    putStrLn pull

gitShortLog :: String -> IO [String]
gitShortLog range =
  lines <$> git "log" ["--pretty=oneline", range]

gitShortLogN :: Int -> Maybe String -> IO [String]
gitShortLogN num mrange =
  lines <$> git "log" (["--max-count=" ++ show num, "--pretty=oneline"] ++ maybeToList mrange)

gitShortLog1 :: Maybe String -> IO String
gitShortLog1 mrange =
  git "log" (["--max-count=1", "--pretty=oneline"] ++ maybeToList mrange)

simplifyCommitLog :: String -> String
simplifyCommitLog = unwords . shortenHash . words
  where
    shortenHash :: [String] -> [String]
    shortenHash [] = []
    shortenHash (h:cs) = take 8 h : cs

gitPushSilent :: IO ()
gitPushSilent = do
  putStr "git pushing... "
  out <- cmdQuiet "git" ["push", "--quiet"]
  putStrLn $ if null out then "done" else "\n" ++ out

workingDirClean :: IO Bool
workingDirClean =
  gitBool "diff-index" ["--quiet", "HEAD"]

checkWorkingDirClean :: IO ()
checkWorkingDirClean = do
  clean <- workingDirClean
  unless clean $ error' "Working dir is not clean"
