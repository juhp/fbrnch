module Cmd.PullPush (
  pullPkgs,
  PullOpts(..),
  fetchPkgs,
  pushPkgs)
where

import Branches
import Common
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

pushPkgs :: (BranchesReq, [String]) -> IO ()
pushPkgs =
  withPackagesByBranches HeaderMay False cleanGitFetch AnyNumber pushPkg
  where
    pushPkg :: Package -> AnyBranch -> IO ()
    pushPkg _pkg _br = do
      gitShortLog1 Nothing >>= putStrLn
      gitPushSilent Nothing
