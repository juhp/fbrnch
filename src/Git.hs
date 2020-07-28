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
  checkIsPkgGitDir,
  isPkgGitDir,
  checkWorkingDirClean,
  isGitDirClean,
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

gitMergeOrigin :: Branch -> IO ()
gitMergeOrigin br = do
  commits <- gitMergeable $ "origin" </> show br
  unless (null commits) $ do
    rebase <- git "rebase" []
    unless ("Already up to date." `isPrefixOf` rebase) $
      putStr rebase

gitShortLog :: String -> IO [String]
gitShortLog range =
  gitLines "log" ["--pretty=oneline", range]

gitShortLogN :: Int -> Maybe String -> IO [String]
gitShortLogN num mrange =
  gitLines "log" (["--max-count=" ++ show num, "--pretty=oneline"] ++ maybeToList mrange)

gitShortLog1 :: Maybe String -> IO String
gitShortLog1 mrange =
  git "log" (["--max-count=1", "--pretty=oneline"] ++ maybeToList mrange)

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
  putStr "git fetching... "
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

checkIsPkgGitDir :: IO ()
checkIsPkgGitDir = do
  pkgGit <- isPkgGitDir
  unless pkgGit $ error' "Not a pkg git dir"

isPkgGitDir :: IO Bool
isPkgGitDir = grepGitConfig "@\\(pkgs\\|src\\)\\."

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
    -- check remote branch exists
    remotebranch <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
    if not remotebranch
      then error' $ show br ++ " branch does not exist!"
      else
      git_ "checkout" ["-q", "-b", show br, "--track", "origin" </> show br]
