module Cmd.Local (installCmd, localCmd, prepCmd, sortCmd) where

import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Package

installCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
installCmd reinstall (mbr,pkgs) = do
  withPackageByBranches True NoGitRepo (installPkg reinstall) (maybeToList mbr,pkgs)

installPkg :: Bool -> Package -> Branch -> IO ()
installPkg reinstall pkg br = do
  spec <- localBranchSpecFile pkg br
  rpms <- builtRpms br spec
  minstalled <- cmdMaybe "rpm" ["-q", unPackage pkg]
  case minstalled of
    Just installed ->
      if not reinstall && installed <.> "rpm" `elem` map takeFileName rpms
      then putStrLn $ installed +-+ "already installed!\n"
      else doInstallPkg spec rpms
    Nothing -> doInstallPkg spec rpms
  where
    doInstallPkg spec rpms = do
      putStrLn $ (takeBaseName . head) rpms ++ "\n"
      missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
      unless (null missingdeps) $
        cmdSilent "sudo" $ "dnf":["builddep", "--assumeyes", spec]
      void $ getSources spec
      buildRPMs True br spec
      putStrLn ""
      sudo_ "dnf" $ (if reinstall then "reinstall" else "install") : "-q" : "-y" : rpms

localCmd :: (Maybe Branch,[String]) -> IO ()
localCmd (mbr,pkgs) =
  withPackageByBranches False NoGitRepo localBuildPkg (maybeToList mbr,pkgs)

localBuildPkg :: Package -> Branch -> IO ()
localBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  buildRPMs False br spec

sortCmd :: (Maybe Branch,[String]) -> IO ()
sortCmd (_,[]) = return ()
sortCmd (mbr,pkgs) = do
  withPackageByBranches True NoGitRepo dummy (maybeToList mbr,pkgs)
  packages <- dependencySort $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br = gitSwitchBranch br

prepCmd :: (Maybe Branch,[String]) -> IO ()
prepCmd (mbr,pkgs) =
  withPackageByBranches True NoGitRepo prepPackage (maybeToList mbr,pkgs)
