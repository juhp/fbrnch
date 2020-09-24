{-# LANGUAGE CPP #-}

module Git (
#if !MIN_VERSION_simple_cmd(0,2,2)
  gitBool,
#endif
  gitLines,
  gitMergeable,
  gitMergeOrigin,
  gitFetchSilent,
  gitPushSilent,
  gitShortLog,
  gitShortLogN,
  gitShortLog1,
  gitSwitchBranch,
  simplifyCommitLog,
--  checkIsPkgGitDir,
  isGitRepo,
  isPkgGitRepo,
  checkWorkingDirClean,
  isGitDirClean,
  CommitOpt (..),
  module SimpleCmd.Git
  ) where

import Common
import Common.System

import SimpleCmd.Git

import Branches

#if !MIN_VERSION_simple_cmd(0,2,2)
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

gitMergeable :: String -> IO [String]
gitMergeable ref = do
  ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", ref]
  if ancestor then gitShortLog $ "HEAD.." ++ ref
    else return []

gitMergeOrigin :: AnyBranch -> IO ()
gitMergeOrigin br = do
  commits <- gitMergeable $ "origin" </> show br
  unless (null commits) $ do
    rebase <- git "rebase" []
    unless ("Already up to date." `isPrefixOf` rebase) $
      putStr rebase

gitShortLog :: String -> IO [String]
gitShortLog range =
  gitLines "log" ["--pretty=reference", range]

gitShortLogN :: Int -> Maybe String -> IO [String]
gitShortLogN num mrange =
  gitLines "log" (["--max-count=" ++ show num, "--pretty=reference"] ++ maybeToList mrange)

gitShortLog1 :: Maybe String -> IO String
gitShortLog1 mrange =
  git "log" (["--max-count=1", "--pretty=reference"] ++ maybeToList mrange)

-- FIXME currently no-op with --format=reference
simplifyCommitLog :: String -> String
simplifyCommitLog = unwords . shortenHash . words
  where
    shortenHash :: [String] -> [String]
    shortenHash [] = []
    shortenHash (h:cs) = take 8 h : cs

gitPushSilent :: Maybe String -> IO ()
gitPushSilent mref = do
  putStr "git pushing... "
  out <- cmdQuiet "git" $ ["push", "--quiet", "origin"] ++ maybeToList mref
  putStrLn $ if null out then "done\n" else "\n" ++ out

gitFetchSilent :: IO ()
gitFetchSilent = do
  name <- getDirectoryName
  putStr $ "git fetching " ++ name ++ "... "
  out <- cmdQuiet "git" ["fetch", "--quiet"]
  putStrLn $ if null out then "done" else "\n" ++ out

checkWorkingDirClean :: IO ()
checkWorkingDirClean = do
  clean <- isGitDirClean
  unless clean $ do
    dir <- getCurrentDirectory
    error' $ "Working dir is not clean: " ++ dir

isGitDirClean :: IO Bool
isGitDirClean =
  gitBool "diff" ["--quiet", "--exit-code", "HEAD"]

-- checkIsPkgGitDir :: IO ()
-- checkIsPkgGitDir = do
--   pkgGit <- isPkgGitRepo
--   unless pkgGit $ error' "Not a pkg git dir"

isGitRepo :: IO Bool
isGitRepo = isGitDir "." ||^ doesFileExist ".git"

isPkgGitRepo :: IO Bool
isPkgGitRepo = grepGitConfig' "@\\(pkgs\\|src\\)\\."
  where
    -- adapted from SimpleCmd.Git
    grepGitConfig' :: String -> IO Bool
    grepGitConfig' key =
      ifM (isGitDir ".")
      (egrep_ key ".git/config") $
      -- could be an absorbed submodule (#8)
      ifM (not <$> doesFileExist ".git")
      (return False) $ do
      -- "gitdir: ../.git/modules/R-bit"
      gitdir <- last . words <$> readFile ".git"
      egrep_ key $ gitdir </> "config"

gitLines :: String -> [String] -> IO [String]
gitLines c args = lines <$> git c args

gitSwitchBranch :: AnyBranch -> IO ()
gitSwitchBranch br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  if show br `elem` localbranches then do
    current <- git "rev-parse" ["--abbrev-ref", "HEAD"]
    when (current /= show br) $
      -- cmdSilent
      git_ "checkout" ["-q", show br]
    else do
    -- check remote branch exists
    remotebranch <-
      ifM checkIfRemoteBranchExists
       (return True) $
        gitFetchSilent >> checkIfRemoteBranchExists
    if not remotebranch
      then do
      name <- getDirectoryName
      error' $ name ++ " " ++ show br ++ " branch does not exist!"
      else
      git_ "checkout" ["-q", "-b", show br, "--track", "origin" </> show br]
  where
    checkIfRemoteBranchExists =
      gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]

data CommitOpt = CommitMsg String | CommitAmend
