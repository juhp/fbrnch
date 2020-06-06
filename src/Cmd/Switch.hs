module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

switchCmd :: Branch -> [Package] -> IO ()
switchCmd br pkgs =
  -- FIXME use withBranchByPackages
  withPackageByBranches LocalBranches dummy ([br],pkgs)
  where
    dummy _ _ = gitSwitchBranch br
