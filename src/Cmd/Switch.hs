module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

-- FIXME noop when on branch already or drop cleanGit
switchCmd :: AnyBranch -> [String] -> IO ()
switchCmd br pkgs =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing dirtyGit Zero dummy (Branches [],pkgs)
  where
    dummy _ _ = gitSwitchBranch br
