module Cmd.Local (installCmd, localCmd) where

import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME --force
installCmd :: (Maybe Branch,[String]) -> IO ()
installCmd (mbr,pkgs) = do
  packages <- dependencySort pkgs
  when (packages /= pkgs) $
    putStrLn $ "Ordered: " ++ unwords packages
  withPackageByBranches NoGitRepo installPkg (maybeToList mbr,packages)

installPkg :: Package -> Branch -> IO ()
installPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  rpms <- rpmsNameVerRel br spec
  buildRPMs br spec
  sudo_ "dnf" $ "install" : rpms

localCmd :: (Maybe Branch,[String]) -> IO ()
localCmd (mbr,pkgs) =
  withPackageByBranches NoGitRepo localBuildPkg (maybeToList mbr,pkgs)

localBuildPkg :: Package -> Branch -> IO ()
localBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  buildRPMs br spec

localBranchSpecFile :: Package -> Branch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitDir
  when gitdir $ do
    putPkgBrnchHdr pkg br
    gitSwitchBranch br
  if gitdir
    then return $ packageSpec pkg
    else findSpecfile
