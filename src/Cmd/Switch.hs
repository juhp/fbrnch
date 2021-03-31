module Cmd.Switch (switchCmd) where

--import Branches
import Git
import Package

-- FIXME noop when on branch already or drop cleanGit
switchCmd :: [String] -> IO ()
switchCmd =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing dirtyGit Nothing True ExactlyOne dummy
  where
    dummy _ br = gitSwitchBranch br
