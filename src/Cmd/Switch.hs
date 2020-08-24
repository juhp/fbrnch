module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

switchCmd :: AnyBranch -> [String] -> IO ()
switchCmd br pkgs =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing cleanGit dummy (anyBranches br,pkgs)
  where
    dummy _ _ = gitSwitchBranch br
