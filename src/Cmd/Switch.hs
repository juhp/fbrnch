module Cmd.Switch (switchCmd) where

--import Branches
import Git
import Package

switchCmd :: [String] -> IO ()
switchCmd =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing cleanGit Nothing oneBranch dummy
  where
    dummy _ br = gitSwitchBranch br
