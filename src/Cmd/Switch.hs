module Cmd.Switch (switchCmd) where

--import Branches
import Git
import Package

switchCmd :: [String] -> IO ()
switchCmd args =
  -- FIXME use withBranchByPackages ?
  withPackageByBranches Nothing cleanGit Nothing oneBranch dummy args
  where
    dummy _ br = gitSwitchBranch br
