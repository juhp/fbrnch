module Cmd.Local (
  installCmd,
  localCmd,
  mockCmd,
  prepCmd,
  sortCmd,
  srpmCmd
  ) where

import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME package countdown
-- FIXME --force to rebuild & install
installCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
installCmd reinstall (mbr,pkgs) = do
  withPackageByBranches False Nothing (installPkg reinstall) (maybeBranches mbr,pkgs)

-- FIXME skip build if rpms newer than spec
installPkg :: Bool -> Package -> Branch -> IO ()
installPkg reinstall pkg br = do
  spec <- localBranchSpecFile pkg br
  rpms <- builtRpms br spec
  -- removing arch
  let pkgs = map takeNVRName rpms
  installed <- filterM pkgInstalled pkgs
  if null installed || (reinstall && length installed /= length pkgs) then
    doInstallPkg spec rpms installed
    else
    putStrLn $ unwords installed +-+ "already installed!\n"
  where
    doInstallPkg spec rpms installed = do
      putStrLn $ (takeBaseName . head) rpms ++ "\n"
      installDeps spec
      void $ getSources spec
      buildRPMs True False br spec
      putStrLn ""
      if reinstall then do
        let reinstalls = filter (\ f -> takeNVRName f `elem` installed) rpms
        unless (null reinstalls) $
          sudo_ "dnf" $ "reinstall" : "-q" : "-y" : reinstalls
        let remaining = rpms \\ reinstalls
        unless (null remaining) $
          sudo_ "dnf" $ "install" : "-q" : "-y" : remaining
        else sudo_ "dnf" $ "install" : "-q" : "-y" : rpms

    takeNVRName = takeBaseName . takeBaseName

installDeps :: FilePath -> IO ()
installDeps spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStr "Running dnf builddep... "
    cmdSilent "sudo" $ "dnf":["builddep", "--assumeyes", spec]
    putStrLn "done"

localCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
localCmd shortcircuit (mbr,pkgs) =
  withPackageByBranches False Nothing localBuildPkg (maybeBranches mbr,pkgs)
  where
    localBuildPkg :: Package -> Branch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      installDeps spec
      void $ getSources spec
      buildRPMs False shortcircuit br spec

srpmCmd :: (Maybe Branch,[String]) -> IO ()
srpmCmd (mbr,pkgs) =
  withPackageByBranches False Nothing srpmBuildPkg (maybeBranches mbr,pkgs)

srpmBuildPkg :: Package -> Branch -> IO ()
srpmBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  void $ generateSrpm (Just br) spec

sortCmd :: (Maybe Branch,[String]) -> IO ()
sortCmd (_,[]) = return ()
sortCmd (mbr,pkgs) = do
  withPackageByBranches False Nothing dummy (maybeBranches mbr,pkgs)
  packages <- dependencySort $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br = gitSwitchBranch br

prepCmd :: (Maybe Branch,[String]) -> IO ()
prepCmd (mbr,pkgs) =
  withPackageByBranches False Nothing prepPackage (maybeBranches mbr,pkgs)

mockCmd :: Maybe Branch -> (Maybe Branch,[String]) -> IO ()
mockCmd mroot (mbr,pkgs) =
  withPackageByBranches True Nothing mockBuildPkg (maybeBranches mbr,pkgs)
  where
    mockBuildPkg :: Package -> Branch -> IO ()
    mockBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      whenM isPkgGitDir $ gitSwitchBranch br
      void $ getSources spec
      srpm <- generateSrpm (Just br) spec
      let resultsdir = "results_" ++ unPackage pkg
          rootBr = fromMaybe br mroot
      cmd_ "mock" ["--root", mockConfig rootBr, "--resultdir=" ++ resultsdir, srpm]
