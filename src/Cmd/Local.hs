module Cmd.Local (
  diffCmd,
  DiffFormat(..),
  DiffWork(..),
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

installCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
installCmd reinstall (mbr,pkgs) = do
  withPackageByBranches True False NoGitRepo (installPkg reinstall) (maybeToList mbr,pkgs)

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
      installDeps spec
      void $ getSources spec
      buildRPMs True False br spec
      putStrLn ""
      sudo_ "dnf" $ (if reinstall then "reinstall" else "install") : "-q" : "-y" : rpms

installDeps :: FilePath -> IO ()
installDeps spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStr "Running dnf builddep... "
    cmdSilent "sudo" $ "dnf":["builddep", "--assumeyes", spec]
    putStrLn "done"

localCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
localCmd shortcircuit (mbr,pkgs) =
  withPackageByBranches False False NoGitRepo localBuildPkg (maybeToList mbr,pkgs)
  where
    localBuildPkg :: Package -> Branch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      installDeps spec
      void $ getSources spec
      buildRPMs False shortcircuit br spec

srpmCmd :: (Maybe Branch,[String]) -> IO ()
srpmCmd (mbr,pkgs) =
  withPackageByBranches False False NoGitRepo srpmBuildPkg (maybeToList mbr,pkgs)

srpmBuildPkg :: Package -> Branch -> IO ()
srpmBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  void $ generateSrpm (Just br) spec

sortCmd :: (Maybe Branch,[String]) -> IO ()
sortCmd (_,[]) = return ()
sortCmd (mbr,pkgs) = do
  withPackageByBranches True False NoGitRepo dummy (maybeToList mbr,pkgs)
  packages <- dependencySort $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br = gitSwitchBranch br

prepCmd :: (Maybe Branch,[String]) -> IO ()
prepCmd (mbr,pkgs) =
  withPackageByBranches True False NoGitRepo prepPackage (maybeToList mbr,pkgs)

mockCmd :: (Maybe Branch,[String]) -> IO ()
mockCmd (mbr,pkgs) =
  withPackageByBranches False False NoGitRepo mockBuildPkg (maybeToList mbr,pkgs)

mockBuildPkg :: Package -> Branch -> IO ()
mockBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  pkggit <- isPkgGitDir
  if pkggit
    then do
    gitSwitchBranch br
    fedpkg_ "mockbuild" []
    else do
    void $ getSources spec
    srpm <- generateSrpm (Just br) spec
    let resultsdir = "results_" ++ unPackage pkg
    cmd_ "mock" ["--root", mockConfig br, "--resultdir=" ++ resultsdir, srpm]

data DiffFormat =
  DiffDefault | DiffShort | DiffContext Int
  deriving (Eq)

data DiffWork =
  DiffWorkAll | DiffWorkUnstage | DiffWorkStaged
  deriving (Eq)

-- FIXME diff branch(es) (without switching?)
diffCmd :: DiffWork -> DiffFormat -> Branch -> [String] -> IO ()
diffCmd work fmt br pkgs =
  withPackageByBranches True False LocalBranches diffPkg ([br],pkgs)
  where
    diffPkg :: Package -> Branch -> IO ()
    diffPkg pkg _br = do
      let contxt = case fmt of
                     DiffContext n -> ["--unified=" ++ show n]
                     _ -> []
          workArgs = case work of
                       DiffWorkAll -> ["HEAD"]
                       DiffWorkUnstage -> []
                       DiffWorkStaged -> ["--cached"]
      diff <- git "diff" $ contxt ++ workArgs
      unless (null diff) $
        case fmt of
          DiffShort -> putStrLn $ unPackage pkg
          _ -> do
            putPkgBrnchHdr pkg br
            putStrLn diff
