module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

-- FIXME noop when on branch already or drop cleanGit
switchCmd :: Bool -> AnyBranch -> [String] -> IO ()
switchCmd verbose br pkgs =
  -- FIXME use withBranchByPackages ?
  withPackagesByBranches HeaderNone False dirtyGit Zero dummy (Branches [],pkgs)
  where
    dummy _ _ = gitSwitchBranchVerbose verbose False br
