module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

switchCmd :: Branch -> [String] -> IO ()
switchCmd br pkgs =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches False True LocalBranches dummy ([br],pkgs)
  where
    dummy _ _ = gitSwitchBranch br
