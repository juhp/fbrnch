module Cmd.PullPush (
  pullPkgs,
  PullOpts(..),
  fetchPkgs,
  pushPkgs)
where

import Branches
import Common
import Common.System (error')
import Git
import Package

data PullOpts =
  PullOpts { pullLenient :: Bool
           , pullNoFetch :: Bool}

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: PullOpts -> (BranchesReq, [String]) -> IO ()
pullPkgs pullopts (breq,args) =
  withPackagesByBranches
  (if length args > 1 then HeaderMust else HeaderMay)
  False
  (if pullLenient pullopts
   then Nothing
   else if pullNoFetch pullopts
        then cleanGit
        else cleanGitFetch)
  AnyNumber pullPkg (breq,args)
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg pkg br =
      if pullLenient pullopts
      then do
        haveGit <- isPkgGitRepo
        if haveGit
          then doPullPkg
          else putStrLn $ "ignoring " ++ unPackage pkg
      else doPullPkg
      where
        doPullPkg :: IO ()
        doPullPkg = do
          current <- getReleaseBranchWarn
          unless (breq == Branches [] || RelBranch current == br) $
            gitSwitchBranch br
          gitMergeOrigin current

fetchPkgs :: [String] -> IO ()
fetchPkgs args =
  withPackagesByBranches
  (if length args > 1 then HeaderMust else HeaderMay)
  False
  dirtyGit
  Zero
  fetchPkg (Branches [],args)
  where
    fetchPkg :: Package -> AnyBranch -> IO ()
    fetchPkg _pkg _br =
      gitFetchSilent False

pushPkgs :: Bool -> Bool -> Maybe String -> (BranchesReq, [String]) -> IO ()
pushPkgs dryrun nofetch mref (breq, pkgs) = do
  when (isJust mref && length pkgs > 1) $
    error' "can only specify ref for single package"
  withPackagesByBranches HeaderMust False (if nofetch then cleanGit else cleanGitFetch) AnyNumber pushPkg (breq, pkgs)
  where
    pushPkg :: Package -> AnyBranch -> IO ()
    pushPkg _pkg _br = do
      whenJustM (gitShortLog1 Nothing) $ putStrLn . showCommit
      if dryrun
        then checkOnBranch
        else gitPush False mref
