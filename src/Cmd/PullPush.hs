module Cmd.PullPush (pullPkgs, pushPkgs) where

import Branches
import Git
import Package

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: (BranchesReq, [String]) -> IO ()
pullPkgs =
  withPackageByBranches (Just False) cleanGitFetch AnyNumber pullPkg
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg _pkg _br =
      getReleaseBranch >>= gitMergeOrigin

pushPkgs :: (BranchesReq, [String]) -> IO ()
pushPkgs =
  withPackageByBranches (Just False) cleanGitFetch AnyNumber pushPkg
  where
    pushPkg :: Package -> AnyBranch -> IO ()
    pushPkg _pkg _br = do
      gitShortLog1 Nothing >>= putStrLn
      gitPushSilent Nothing
