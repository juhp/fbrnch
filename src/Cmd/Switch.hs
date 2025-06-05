module Cmd.Switch (switchCmd) where

import Control.Monad (unless)

import Branches
import Common.System
import Git
import Package

-- FIXME noop when on branch already or drop cleanGit
switchCmd :: Bool -> Bool -> AnyBranch -> [String] -> IO ()
switchCmd verbose lenient br pkgs =
  -- FIXME use withBranchByPackages ?
  withPackagesByBranches HeaderNone False dirtyGit Zero dummy (Branches [],pkgs)
  where
    dummy pkg _ =
      if lenient
      then do
        let rbr = onlyRelBranch br
        ok <- gitSwitchBranch' (not verbose) rbr
        unless ok $ warning $ unPackage pkg +-+ "missing" +-+ showBranch rbr
      else gitSwitchBranchVerbose verbose False br
