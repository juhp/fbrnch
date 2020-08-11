module Cmd.Pull (pullPkgs) where

import Branches
import Git
import Package

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: (Maybe Branch,[String]) -> IO ()
pullPkgs (mbr,pkgs) =
  withPackageByBranches True cleanGitFetch pullPkg (maybeBranches mbr,pkgs)
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg _pkg _br =
      gitCurrentBranch >>= gitMergeOrigin
