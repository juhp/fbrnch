module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

-- FIXME noop when on branch already or drop cleanGit
switchCmd :: AnyBranch -> [String] -> IO ()
switchCmd br =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing dirtyGit Nothing ExactlyOne dummy [br]
  where
    dummy _ _ = gitSwitchBranch br
