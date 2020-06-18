module Cmd.Local (installCmd, localCmd) where

import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Package

installCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
installCmd reinstall (mbr,pkgs) = do
  packages <- dependencySort pkgs
  when (packages /= pkgs) $
    putStrLn $ "Ordered: " ++ unwords packages
  withPackageByBranches NoGitRepo (installPkg reinstall) (maybeToList mbr,packages)

installPkg :: Bool -> Package -> Branch -> IO ()
installPkg reinstall pkg br = do
  spec <- localBranchSpecFile pkg br
  rpms <- rpmsNameVerRel br spec
  buildRPMs br spec
  sudo_ "dnf" $ (if reinstall then "reinstall" else "install") : rpms

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
