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
  gitSwitchBranchVerbose,
--  checkIsPkgGitDir,
  isGitRepo,
  isPkgGitRepo,
  isPkgGitSshRepo,
  checkWorkingDirClean,
  gitUnstash,
  stashedWithFbrnch,
  isGitDirClean,
  checkIfRemoteBranchExists,
  CommitOpt (..),
  refPrompt,
  conflictPrompt,
  module SimpleCmd.Git
  ) where

import Data.Char (isSpace)
import Distribution.Fedora.Branch (newerBranch)
import Safe (tailSafe)
import Say (sayString)
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

-- Just True => ancestor
-- Nothing => neither ancestor
-- Just False => reverse ancestor
gitMergeable :: Branch -> Branch -> IO (Maybe Bool,[Commit])
gitMergeable target br = do
  let ref = "origin/" ++ showBranch br
  mancestor <- do
    ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", ref]
    if ancestor
      then return $ Just True
      else do
      revancestor <- gitBool "merge-base" ["--is-ancestor", ref, "HEAD"]
      if revancestor
        then return $ Just False
        else return Nothing
  commits <- gitOneLineLog ("HEAD.." ++ ref)
  when (br /= target && null commits && mancestor /= Just True) $
    if mancestor == Just False
      then do
      diff <- git "diff" [ref]
      unless (null diff) $ do
        putStrLn $ "current branch is ahead of newer" +-+ showBranch br +-+ "!!"
        promptEnter "Press Enter if you want to continue"
      else putStrLn $ "current branch" +-+ "is diverged from" +-+ showBranch br
  return (mancestor, commits)

-- FIXME use Package
getNewerBranch :: String -> Branch -> IO (Maybe Branch)
getNewerBranch _ Rawhide = return Nothing
getNewerBranch pkg br = do
  localbrs <- fedoraBranches (localBranches False)
  case newerBranch br localbrs of
    Just newer ->
      if newer `elem` localbrs
      then return $ Just newer
      else do
        remotebrs <- fedoraBranches (pagurePkgBranches pkg)
        if newer `elem` remotebrs
          then do
          gitFetchSilent False
          return $ Just newer
          else return $ newerBranch br remotebrs
    Nothing -> return Nothing

gitMergeOrigin :: Branch -> IO ()
gitMergeOrigin br = do
  (mancestor,commits) <- gitMergeable br br
  when (mancestor == Just True) $
    unless (null commits) $ do
    pull <- git "pull" []
    unless ("Already up to date." `isPrefixOf` pull) $
      putStrLn pull

-- FIXME maybe require local branch already here
-- FIXME also expose local commits
newerMergeable :: String -> Branch -> IO (Bool,[Commit],Maybe Branch)
newerMergeable pkg br =
  if br == Rawhide
  then return (False,[],Nothing)
  else do
    mnewer <- getNewerBranch pkg br
    case mnewer of
      Just newer -> do
        (mancestor,commits) <- gitMergeable br newer
        return (mancestor == Just True, commits, Just newer)
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
          Commit hash (init $ tailSafe $ trim plogcs) (init datep)

gitPush :: Bool -> Maybe String -> IO ()
gitPush quiet mref = do
  -- FIXME also check ref on branch
  checkOnBranch
  when quiet $
    sayString "git pushing"
  -- Can error like this:
  -- kex_exchange_identification: Connection closed by remote host
  -- Connection closed by 38.145.60.17 port 22
  -- fatal: Could not read from remote repository.
  let args = ["push"] ++ ["--quiet" | quiet] ++ ["origin"] ++ maybeToList mref
  (ok, _out, err) <- cmdFull "git" args ""
  if ok
    then unless quiet $
         putStrLn $ last (lines err)
    else do
    when quiet putNewLn
    putStrLn $ unwords ("git" : args) +-+ "failed with\n" ++ err
    yes <- yesNo "Retry git push"
    -- FIXME going to fail if ref no longer on branch
    when yes $ gitPush quiet mref

-- FIXME use this in more places
gitRepoName :: IO String
gitRepoName =
  dropSuffix ".git" . takeFileName <$> git "remote" ["get-url", "origin"]

-- FIXME use Verbose
gitFetchSilent :: Bool -> IO ()
gitFetchSilent quiet = do
  name <- gitRepoName
  unless quiet $
    putStr $ "git fetching" +-+ name ++ "... "
  (ok, out, err) <- cmdFull "git" ["fetch"] ""
  unless quiet $
    unless (null out) $ putStrLn out
  unless ok $ error' err
  -- could keep From if no header
  let filtered = case lines err of
        [] -> []
        (hd:tl) -> filter (/= "Already up to date.") $
                   if "From " `isPrefixOf` hd then tl else hd:tl
  if null filtered
    then unless quiet $ putStrLn "done"
    else putStrLn $ '\r' : intercalate "\n" filtered

stashedWithFbrnch :: String
stashedWithFbrnch = "Saved by fbrnch"

checkWorkingDirClean :: Bool -> IO ()
checkWorkingDirClean stash = do
  clean <- isGitDirClean
  unless clean $
    if stash
    then git_ "stash" ["-m", stashedWithFbrnch]
    else do
      dir <- getCurrentDirectory
      error' $ "Working dir is not clean:" +-+ dir

-- FIXME add quiet option?
gitUnstash :: IO ()
gitUnstash = do
  mstash <- listToMaybe <$> gitLines "stash" ["list"]
  whenJust mstash $ \stash ->
    when (stashedWithFbrnch `isSuffixOf` stash) $
    git_ "stash" ["pop", "--quiet"]

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
grepGitConfig' key = do
  isgit <- isGitDir "."
  if isgit
    then egrep_ key ".git/config"
    else do
    -- could be a worktree or absorbed submodule (#8)
    exists <- doesFileExist ".git"
    if not exists
      then return False
      else do
      gitdir <- last . words <$> readFile ".git"
      if "/worktrees/" `isInfixOf` gitdir
        then egrep_ key (takeDirectory (takeDirectory gitdir) </> "config")
        else
        -- absorbed submodule: "gitdir: ../.git/modules/R-bit"
        if "/modules/" `isInfixOf` gitdir then
          egrep_ key $ gitdir </> "config"
          else return False

gitSwitchBranchVerbose :: Bool -> Bool -> AnyBranch -> IO ()
gitSwitchBranchVerbose _ allowHEAD (OtherBranch "HEAD") = do
  dir <- getDirectoryName
  (if allowHEAD then putStrLn else error') $ dir ++ ": HEAD is not a branch"
gitSwitchBranchVerbose verbose _ br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  let verb = ["-q" | not verbose]
  if show br `elem` localbranches
    then do
    current <- gitCurrentBranch
    when (current /= br) $
      git_ "switch" $ verb ++ [show br]
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
      error' $ name +-+ show br +-+ "branch does not exist!"
      else
      git_ "checkout" $ verb ++ ["-b", show br, "--track", "origin/" ++ show br]

gitSwitchBranch :: AnyBranch -> IO ()
gitSwitchBranch = gitSwitchBranchVerbose False False

-- similar to gitSwitchBranch but does not error
gitSwitchBranch' :: Bool -> Branch -> IO Bool
gitSwitchBranch' quiet br = do
  localbranches <- gitLines "branch" ["--format=%(refname:short)"]
  if showBranch br `elem` localbranches
    then do
    current <- gitCurrentBranch
    when (current /= RelBranch br) $
      git_ "switch" ["-q", showBranch br]
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
        warning $ name +-+ showBranch br +-+ "branch does not exist!"
      return False
      else do
      git_ "checkout" ["-q", "-b", showBranch br, "--track", "origin/" ++ showBranch br]
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
      -- FIXME use promptMap
      ref <- prompt txt
      case lower ref of
        "" -> return $ Just c
        "y" -> return $ Just c
        "yes" -> return $ Just c
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
