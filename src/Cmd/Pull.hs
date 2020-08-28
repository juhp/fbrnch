module Cmd.Pull (pullPkgs) where

import Branches
import Git
import Package

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: [String] -> IO ()
pullPkgs args =
  withPackageByBranches (Just False) cleanGitFetch Nothing Nothing pullPkg args
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg _pkg _br =
      gitCurrentBranch >>= gitMergeOrigin
