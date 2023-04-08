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
  gitPush,
  gitRepoName,
  Commit(commitRef,commitLog),
  showCommit,
  displayCommits,
  gitOneLineLog,
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
  refPrompt,
  conflictPrompt,
  module SimpleCmd.Git
  ) where

import Data.Char (isSpace)
import SimpleCmd.Git
import SimplePrompt

import Branches
import Common
import Common.System

#if !MIN_VERSION_simple_cmd(0,2,2)
-- | 'gitBool c args' runs git command and return result
gitBool :: String -- ^ git command
        -> [String] -- ^ arguments
        -> IO Bool -- ^ result
gitBool c args =
  cmdBool "git" (c:args)
#endif

gitMergeable :: Bool -> Branch -> IO (Bool,[Commit])
gitMergeable origin br = do
  let ref = (if origin then "origin/" else "") ++ show br
  ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", ref]
  commits <- gitOneLineLog ("HEAD.." ++ ref)
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
    else
      case elemIndex br branches of
        Just i -> Just $ branches !! (i - 1)
        Nothing -> Nothing

gitMergeOrigin :: Branch -> IO ()
gitMergeOrigin br = do
  (ancestor,commits) <- gitMergeable True br
  when ancestor $
    unless (null commits) $ do
    pull <- git "pull" []
    unless ("Already up to date." `isPrefixOf` pull) $
      putStr pull

-- FIXME maybe require local branch already here
newerMergeable :: Branch -> IO (Bool,[Commit],Maybe Branch)
newerMergeable br =
  if br == Rawhide
  then return (False,[],Nothing)
  else do
    mnewer <- getNewerBranch br
    locals <- localBranches True
    case mnewer of
      Just newer -> do
        (ancestor,commits) <- gitMergeable (show newer `notElem` locals) newer
        return (ancestor, commits, Just newer)
      Nothing -> return (False,[],Nothing)

data Commit = Commit
              { commitRef :: String,
                commitLog :: String,
                commitDate :: String }

showCommit :: Commit -> String
showCommit c =
  take 7 (commitRef c) +-+ commitLog c +-+ "(" ++ commitDate c ++ ")"

displayCommits :: Bool -> [Commit] -> IO ()
displayCommits showall =
  mapM_ putStrLn . showAll showall . map showCommit
  where
    showAll :: Bool -> [String] -> [String]
    showAll False cs =
      if length cs > 20 then take 20 cs ++ [":"] else cs
    showAll True cs = cs

gitOneLineLog :: String -> IO [Commit]
gitOneLineLog range =
  map mkCommit <$> gitLines "log" ["--pretty=format:%H (%s, %cs)", range]

gitShortLogN :: Maybe Int -> Maybe String -> IO [Commit]
gitShortLogN mnum mrange =
  map mkCommit <$> gitLines "log" (["--max-count=" ++ show num | num <- maybeToList mnum] ++ "--pretty=reference": maybeToList mrange)

gitShortLog1 :: Maybe String -> IO (Maybe Commit)
gitShortLog1 mrange = do
  cs <- git "log" (["--max-count=1", "--pretty=reference"] ++ maybeToList mrange)
  return $
    if null cs
    then Nothing
    else Just $ mkCommit cs

-- assumes reference style pretty format: "hash (title, date)"
mkCommit :: String -> Commit
mkCommit cs =
  case word1 cs of
    ("",_) -> error' "empty commit log line!"
    (hash,rest) ->
      case breakEnd isSpace rest of
        -- "(msg txt, date)"
        (plogcs,datep) ->
          Commit hash (init $ tail $ trim plogcs) (init datep)

gitPush :: Bool -> Maybe String -> IO ()
gitPush quiet mref = do
  checkOnBranch
  when quiet $
    putStr "git pushing... "
  -- Can error like this:
  -- kex_exchange_identification: Connection closed by remote host
  -- Connection closed by 38.145.60.17 port 22
  -- fatal: Could not read from remote repository.
  let args = ["push"] ++ ["--quiet" | quiet] ++ ["origin"] ++ maybeToList mref
  (ok, _out, err) <- cmdFull "git" args ""
  if ok
    then putStrLn $ if quiet then "done" else last (lines err)
    else do
    when quiet $ putStrLn ""
    putStrLn $ unwords ("git" : args) +-+ "failed with\n" ++ err
    yes <- yesno Nothing "Retry git push"
    when yes $ gitPush quiet mref

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
    else putStrLn $ intercalate "\n" filtered

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
      unless quiet $
        warning $ name ++ " " ++ show br ++ " branch does not exist!"
      return False
      else do
      git_ "checkout" ["-q", "-b", show br, "--track", "origin/" ++ show br]
      return True

checkIfRemoteBranchExists :: AnyBranch -> IO Bool
checkIfRemoteBranchExists br =
  gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]

data CommitOpt = CommitMsg String | CommitAmend

-- FIXME select ref by number
-- FIXME minimum length of hash
refPrompt :: [Commit] -> String -> IO (Maybe String)
refPrompt commits txt = do
  case map commitRef commits of
    [] -> error' "empty commits list"
    (c:cs) -> do
      ref <- prompt txt
      case lower ref of
        "" -> return $ Just c
        "no" -> return Nothing
        "n" -> return Nothing
        _ ->
          case find (ref `isPrefixOf`) cs of
            Just cref -> return $ Just cref
            Nothing -> refPrompt commits txt

-- FIXME also include branch
-- FIXME minimum length of hash
conflictPrompt :: [Commit] -> String -> IO (Maybe String)
conflictPrompt commits txt = do
  case map commitRef commits of
    [] -> error' "empty commits list"
    commitrefs@(c:_) -> do
      ref <- prompt txt
      if null ref
        then return Nothing
        else
        case find (ref `isPrefixOf`) commitrefs of
          Just cref -> return $ Just cref
          Nothing ->
            if lower ref == "head"
            then return $ Just c
            else conflictPrompt commits txt
