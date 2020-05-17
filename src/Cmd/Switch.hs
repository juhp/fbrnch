module Cmd.Switch (switchCmd) where

import Branches
import Git
import Package

switchCmd :: Branch -> [Package] -> IO ()
switchCmd br pkgs =
  withPackageBranches False dummy ([br],pkgs)
  where
    dummy _ _ = gitSwitchBranch br
