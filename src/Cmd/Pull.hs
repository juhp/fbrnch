module Cmd.Pull (pullPkgs) where

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
      gitCurrentBranch >>= gitMergeOrigin
