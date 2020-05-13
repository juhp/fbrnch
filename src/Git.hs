{-# LANGUAGE CPP #-}

module Git (
  gitBool,
  gitPull,
  gitPushSilent,
  gitShortLog,
  gitShortLogN,
  gitShortLog1,
  gitSwitchBranch,
  simplifyCommitLog,
  checkIsPkgGitDir,
  checkWorkingDirClean,
  module SimpleCmd.Git
  ) where

import Common
import Common.System

import SimpleCmd.Git

import Branches

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,2,2))
#else
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

gitRemoteBranched :: Branch -> IO Bool
gitRemoteBranched br =
  gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]

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

checkWorkingDirClean :: IO ()
checkWorkingDirClean = do
  clean <- gitBool "diff-index" ["--quiet", "HEAD"]
  unless clean $ error' "Working dir is not clean"

-- FIXME check actually pkg dist-git
checkIsPkgGitDir :: IO ()
checkIsPkgGitDir = do
  isGit <- doesDirectoryExist ".git"
  unless isGit $ error' "Not a git dir"

gitLines :: String -> [String] -> IO [String]
gitLines c args = lines <$> git c args

gitSwitchBranch :: Branch -> IO ()
gitSwitchBranch br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  if show br `elem` localbranches then do
    current <- git "rev-parse" ["--abbrev-ref", "HEAD"]
    when (current /= show br) $
      -- cmdSilent
      git_ "checkout" ["-q", show br]
    else do
    remotebranch <- gitRemoteBranched br
    if not remotebranch
      then error' $ show br ++ " branch does not exist!"
      else
      git_ "checkout" ["-q", "-b", show br, "--track", "origin" </> show br]
