module Cmd.Local (
  installCmd,
  localCmd,
  mockCmd,
  prepCmd,
  sortCmd,
  scratchCmd,
  srpmCmd
  ) where

import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Koji
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
      installDeps spec
      void $ getSources spec
      buildRPMs True br spec
      putStrLn ""
      sudo_ "dnf" $ (if reinstall then "reinstall" else "install") : "-q" : "-y" : rpms

installDeps :: FilePath -> IO ()
installDeps spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStr "Running dnf builddep... "
    cmdSilent "sudo" $ "dnf":["builddep", "--assumeyes", spec]
    putStrLn "done"

localCmd :: (Maybe Branch,[String]) -> IO ()
localCmd (mbr,pkgs) =
  withPackageByBranches False NoGitRepo localBuildPkg (maybeToList mbr,pkgs)

localBuildPkg :: Package -> Branch -> IO ()
localBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  installDeps spec
  void $ getSources spec
  buildRPMs False br spec

srpmCmd :: (Maybe Branch,[String]) -> IO ()
srpmCmd (mbr,pkgs) =
  withPackageByBranches False NoGitRepo srpmBuildPkg (maybeToList mbr,pkgs)

srpmBuildPkg :: Package -> Branch -> IO ()
srpmBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  void $ generateSrpm (Just br) spec

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

mockCmd :: (Maybe Branch,[String]) -> IO ()
mockCmd (mbr,pkgs) =
  withPackageByBranches False NoGitRepo mockBuildPkg (maybeToList mbr,pkgs)

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

-- FIXME --arches
-- FIXME --[no-]rebuild-srpm for scratch
-- FIXME --exclude-arch
scratchCmd :: Bool -> Maybe String -> Maybe String -> (Maybe Branch,[String]) -> IO ()
scratchCmd nofailfast march mtarget (mbr,pkgs) =
  withPackageByBranches False NoGitRepo (scratchBuild nofailfast march mtarget) (maybeToList mbr,pkgs)

scratchBuild :: Bool -> Maybe String -> Maybe String -> Package -> Branch -> IO ()
scratchBuild nofailfast march mtarget pkg br = do
  spec <- localBranchSpecFile pkg br
  let target = fromMaybe "rawhide" mtarget
  let args = ["--arch-override=" ++ fromJust march | isJust march] ++ ["--fail-fast" | not nofailfast]
  pkggit <- isPkgGitDir
  if pkggit
    then do
    gitSwitchBranch br
    pushed <- do
      clean <- isGitDirClean
      if clean then
        null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
        else return False
    if pushed then do
      void $ getSources spec
      kojiBuildBranch target (unPackage pkg) $ ["--scratch"] ++ args
      else srpmBuild target args spec
    else srpmBuild target args spec
  where
    srpmBuild :: FilePath -> [String] -> String -> IO ()
    srpmBuild target args spec = do
      void $ getSources spec
      void $ generateSrpm (Just br) spec >>= kojiScratchBuild target args
