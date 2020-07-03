module Cmd.Local (installCmd, localCmd, prepCmd, sortCmd, scratchCmd) where

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

scratchCmd :: Bool -> Maybe String -> (Maybe Branch,[String]) -> IO ()
scratchCmd nofailfast mtarget (mbr,pkgs) =
  withPackageByBranches False NoGitRepo (scratchBuild nofailfast mtarget) (maybeToList mbr,pkgs)

-- FIXME --arch
scratchBuild :: Bool -> Maybe String -> Package -> Branch -> IO ()
scratchBuild nofailfast mtarget pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  let target = fromMaybe "rawhide" mtarget
  pkggit <- isPkgGitDir
  if pkggit
    then do
    gitSwitchBranch br
    pushed <- do
      clean <- isGitDirClean
      if clean then
        null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
        else return False
    if pushed then
      kojiBuildBranch target (unPackage pkg) $ ["--fail-fast" | not nofailfast] ++ ["--scratch"] -- ++ march
      else srpmBuild spec target
    else srpmBuild spec target
  where
    srpmBuild :: FilePath -> String -> IO ()
    srpmBuild spec target =
      void $ generateSrpm (Just br) spec >>= kojiScratchBuild target
