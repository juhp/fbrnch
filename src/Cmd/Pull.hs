module Cmd.Pull (pullPkgs) where

import Branches
import Git
import Package

-- FIXME pulling more than one branch
pullPkgs :: (Maybe Branch,[String]) -> IO ()
pullPkgs (mbr,pkgs) =
  withPackageByBranches True cleanGitFetch pullPkg (maybeBranches mbr,pkgs)
  where
    pullPkg :: Package -> Branch -> IO ()
    pullPkg _pkg _br =
      gitCurrentBranch >>= gitMergeOrigin
