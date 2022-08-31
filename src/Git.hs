{-# LANGUAGE CPP #-}

module Git (
#if !MIN_VERSION_simple_cmd(0,2,2)
  gitBool,
#endif
  gitLines,
  gitMergeable,
  gitMergeOrigin,
  getNewerBranch,
  newerMergeable,
  gitFetchSilent,
  gitPushSilent,
  gitRepoName,
  gitShortLog,
  gitShortLogN,
  gitShortLog1,
  gitSwitchBranch,
  gitSwitchBranch',
--  checkIsPkgGitDir,
  isGitRepo,
  isPkgGitRepo,
  isPkgGitSshRepo,
  checkWorkingDirClean,
  isGitDirClean,
  checkIfRemoteBranchExists,
  CommitOpt (..),
  module SimpleCmd.Git
  ) where

import SimpleCmd.Git

import Branches
import Common
import Common.System
import Prompt

#if !MIN_VERSION_simple_cmd(0,2,2)
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

gitMergeable :: Bool -> Branch -> IO (Bool,[String])
gitMergeable origin br = do
  let ref = (if origin then "origin/" else "") ++ show br
  ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", ref]
  commits <- gitShortLog ("HEAD.." ++ ref)
  when (not origin && null commits && not ancestor) $
    whenM (gitBool "merge-base" ["--is-ancestor", "HEAD", "origin/" ++ show br]) $ do
    rancestor <- gitBool "merge-base" ["--is-ancestor", ref, "HEAD"]
    if rancestor
      then do
      diff <- git "diff" [ref]
      unless (null diff) $ do
        putStrLn $ "current branch is ahead of newer" +-+ show br +-+ "!!"
        prompt_ "Press Enter if you want to continue"
      else putStrLn $ "current branch" +-+ "is diverged from" +-+ show br
  return (ancestor, commits)

getNewerBranch :: Branch -> IO (Maybe Branch)
getNewerBranch Rawhide = return Nothing
getNewerBranch br = do
  branches <- fedoraBranches (localBranches False)
  let newer = newerBranch br branches
  return $
    if newer > br
    then Just newer
    else Nothing
    -- -- FIXME? this can be dropped with next fedora-dists
    -- else case elemIndex br branches of
    --        Just i -> branches !! (i - 1)
    --        Nothing -> error' $ show br ++ ": branch not found"

gitMergeOrigin :: Branch -> IO ()
gitMergeOrigin br = do
  (ancestor,commits) <- gitMergeable True br
  when ancestor $
    unless (null commits) $ do
    pull <- git "pull" []
    unless ("Already up to date." `isPrefixOf` pull) $
      putStr pull

-- FIXME maybe require local branch already here
newerMergeable :: Branch -> IO (Bool,[String])
newerMergeable br =
  if br == Rawhide
  then return (False,[])
  else do
    mnewer <- getNewerBranch br
    locals <- localBranches True
    case mnewer of
      Just newer -> gitMergeable (show newer `notElem` locals) newer
      Nothing -> return (False,[])

gitShortLog :: String -> IO [String]
gitShortLog range =
  map simplifyCommitLog <$> gitLines "log" ["--pretty=reference", range]

gitShortLogN :: Int -> Maybe String -> IO [String]
gitShortLogN num mrange =
  map simplifyCommitLog <$> gitLines "log" (["--max-count=" ++ show num, "--pretty=reference"] ++ maybeToList mrange)

gitShortLog1 :: Maybe String -> IO String
gitShortLog1 mrange =
  simplifyCommitLog <$> git "log" (["--max-count=1", "--pretty=reference"] ++ maybeToList mrange)

-- FIXME currently no-op with --format=reference
simplifyCommitLog :: String -> String
simplifyCommitLog = unwords . shortenHash . words
  where
    shortenHash :: [String] -> [String]
    shortenHash [] = []
    shortenHash (h:cs) = take 8 h : simplifyLog cs

    simplifyLog [] = []
    -- remove leading '('
    simplifyLog (w:ws) =
      case ws of
        [] -> error "malformed changelog"
        [_] -> init (tail w) : map ('(' :) ws
        _ ->
          let (mid,end) = splitAt (length ws - 2) ws
          in tail w : mid ++ [init (head end),'(' : last end]

gitPushSilent :: Maybe String -> IO ()
gitPushSilent mref = do
  checkOnBranch
  putStr "git pushing... "
  out <- cmdQuiet "git" $ ["push", "--quiet", "origin"] ++ maybeToList mref
  putStrLn $ if null out then "done" else "\n" ++ out

-- FIXME use this in more places
gitRepoName :: IO String
gitRepoName =
  dropSuffix ".git" . takeFileName <$> git "remote" ["get-url", "origin"]

gitFetchSilent :: Bool -> IO ()
gitFetchSilent quiet = do
  name <- gitRepoName
  unless quiet $
    putStr $ "git fetching " ++ name ++ "... "
  (ok, out, err) <- cmdFull "git" ["fetch"] ""
  unless (null out) $ putStrLn out
  unless ok $ error' err
  let filtered = case lines err of
        [] -> []
        (hd:tl) -> filter (/= "Already up to date.") $
                   if "From " `isPrefixOf` hd then tl else hd:tl
  if null filtered
    then unless quiet $ putStrLn "done"
    else putStrLn $ "\n" ++ intercalate "\n" filtered

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
isPkgGitRepo = grepGitConfig' "\\(https://\\|@\\)\\(pkgs\\|src\\)\\."
               &&^
               (not . ("/forks/" `isInfixOf`) <$>
                git "config" ["--get", "remote.origin.url"])

isPkgGitSshRepo :: IO Bool
isPkgGitSshRepo = grepGitConfig' "@\\(pkgs\\|src\\)\\."

-- adapted from SimpleCmd.Git
grepGitConfig' :: String -> IO Bool
grepGitConfig' key =
  ifM (isGitDir ".")
  (egrep_ key ".git/config") $
  -- could be a worktree or absorbed submodule (#8)
  ifM (not <$> doesFileExist ".git")
  (return False) $ do
  gitdir <- last . words <$> readFile ".git"
  if "/worktrees/" `isInfixOf` gitdir
    then egrep_ key (takeDirectory (takeDirectory gitdir) </> "config")
    else
    -- absorbed submodule: "gitdir: ../.git/modules/R-bit"
    if "/modules/" `isInfixOf` gitdir then
      egrep_ key $ gitdir </> "config"
      else return False

gitLines :: String -> [String] -> IO [String]
gitLines c args = lines <$> git c args

gitSwitchBranch :: AnyBranch -> IO ()
gitSwitchBranch (OtherBranch "HEAD") = do
  dir <- getDirectoryName
  error' $ dir ++ ": HEAD is not a branch"
gitSwitchBranch br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  if show br `elem` localbranches
    then do
    current <- gitCurrentBranch
    when (current /= br) $
      git_ "switch" ["-q", show br]
    else do
    -- check remote branch exists
    remotebranch <- do
      exists <- checkIfRemoteBranchExists br
      if exists
      then return True
      else gitFetchSilent False >> checkIfRemoteBranchExists br
    if not remotebranch
      then do
      name <- getDirectoryName
      error' $ name ++ " " ++ show br ++ " branch does not exist!"
      else
      git_ "checkout" ["-q", "-b", show br, "--track", "origin/" ++ show br]

-- similar to gitSwitchBranch but does not error
gitSwitchBranch' :: Bool -> Branch -> IO Bool
gitSwitchBranch' quiet br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  if show br `elem` localbranches
    then do
    current <- gitCurrentBranch
    when (current /= RelBranch br) $
      git_ "switch" ["-q", show br]
    return True
    else do
    -- check remote branch exists
    remotebranch <- do
      exists <- checkIfRemoteBranchExists (RelBranch br)
      if exists
        then return True
        -- FIXME this is redundant if we already fetched (eg for merge cmd)
        else gitFetchSilent quiet >> checkIfRemoteBranchExists (RelBranch br)
    if not remotebranch
      then do
      name <- getDirectoryName
      warning $ name ++ " " ++ show br ++ " branch does not exist!"
      return False
      else do
      git_ "checkout" ["-q", "-b", show br, "--track", "origin/" ++ show br]
      return True

checkIfRemoteBranchExists :: AnyBranch -> IO Bool
checkIfRemoteBranchExists br =
  gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]

data CommitOpt = CommitMsg String | CommitAmend
