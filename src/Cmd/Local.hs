module Cmd.Local (installCmd, localCmd) where

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME --force
installCmd :: (Maybe Branch,[String]) -> IO ()
installCmd (mbr,pkgs) =
  withPackageBranches NoGitRepo installPkg (maybeToList mbr,pkgs)

installPkg :: String -> Branch -> IO ()
installPkg pkg br = do
  gitdir <- isPkgGitDir
  when gitdir $
    gitSwitchBranch br
  spec <- if gitdir then return $ pkg <.> "spec"
          else findSpecfile
  rpms <- rpmsNameVerRel br spec
  fedpkg_ "local" []
  sudo_ "dnf" $ "install" : rpms

localCmd :: (Maybe Branch,[String]) -> IO ()
localCmd (mbr,pkgs) =
  withPackageBranches NoGitRepo localBuildPkg (maybeToList mbr,pkgs)

localBuildPkg :: String -> Branch -> IO ()
localBuildPkg pkg br = do
  gitdir <- isPkgGitDir
  when gitdir $
    gitSwitchBranch br
  spec <- if gitdir then return $ pkg <.> "spec"
          else findSpecfile
  cmd_ "rpmbuild" ["-bb", spec]
