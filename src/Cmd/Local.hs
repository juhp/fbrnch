module Cmd.Local (
  commandCmd,
  installCmd,
  installDepsCmd,
  localCmd,
  nvrCmd,
  prepCmd,
  sortCmd,
  RpmWith(..),
  srpmCmd,
  masterRenameCmd
  ) where

import Distribution.RPM.Build.Order (dependencySortRpmOpts)
import qualified System.Process as P
import System.Environment

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME package countdown
-- FIXME --ignore-uninstalled subpackages
-- FIXME --recursive
installCmd :: Maybe ForceShort -> [BCond] -> Bool -> [String] -> IO ()
installCmd mforceshort bconds reinstall =
  withPackageByBranches Nothing Nothing Nothing True ZeroOrOne installPkg
  where
    installPkg :: Package -> AnyBranch -> IO ()
    installPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- builtRpms br spec
      -- removing arch
      let packages = map takeNVRName rpms
      installed <- filterM pkgInstalled packages
      if isJust mforceshort || null installed || reinstall
        then doInstallPkg spec rpms installed
        else putStrLn $ unwords installed ++ " already installed!\n"
      where
        doInstallPkg spec rpms installed = do
          putStrLn $ takeBaseName (head rpms) ++ "\n"
          buildRPMs True mforceshort bconds rpms br spec
          putStrLn ""
          unless (mforceshort == Just ShortCircuit) $
            if reinstall then do
              let reinstalls = filter (\ f -> takeNVRName f `elem` installed) rpms
              unless (null reinstalls) $
                sudo_ "/usr/bin/dnf" $ "reinstall" : "-q" : "-y" : reinstalls
              let remaining = filterDebug $ rpms \\ reinstalls
              unless (null remaining) $
                sudo_ "/usr/bin/dnf" $ "install" : "-q" : "-y" : remaining
              else sudo_ "/usr/bin/dnf" $ "install" : "-q" : "-y" : filterDebug rpms

        filterDebug = filter (\p -> not (any (`isInfixOf` p) ["-debuginfo-", "-debugsource-"]))

localCmd :: Maybe ForceShort -> [BCond] -> [String] -> IO ()
localCmd mforceshort bconds =
  withPackageByBranches Nothing Nothing Nothing True ZeroOrOne localBuildPkg
  where
    localBuildPkg :: Package -> AnyBranch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- builtRpms br spec
      buildRPMs False mforceshort bconds rpms br spec

-- FIXME single branch
installDepsCmd :: [String] -> IO ()
installDepsCmd =
  withPackageByBranches Nothing Nothing Nothing True ZeroOrOne installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps

-- FIXME single branch
srpmCmd :: [String] -> IO ()
srpmCmd =
  withPackageByBranches Nothing Nothing Nothing True ZeroOrOne srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm (Just br) spec

data RpmWith = RpmWith String | RpmWithout String

sortCmd :: Maybe RpmWith -> [String] -> IO ()
sortCmd _ [] = return ()
sortCmd mrpmwith args = do
  (brs,pkgs) <- splitBranchesPkgs False Nothing True args
  withPackageByBranches' Nothing Nothing Nothing ExactlyOne dummy (brs,pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  packages <- dependencySortRpmOpts rpmopts $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br =
      whenM isPkgGitRepo $ gitSwitchBranch br

    toRpmOption :: RpmWith -> [String]
    toRpmOption (RpmWith opt) = ["--with=" ++ opt]
    toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

prepCmd :: [String] -> IO ()
prepCmd =
  withPackageByBranches Nothing Nothing Nothing True ZeroOrOne prepPackage

nvrCmd :: Maybe BranchOpts -> [String] -> IO ()
nvrCmd mbrnchopts =
  withPackageByBranches Nothing Nothing mbrnchopts True AnyNumber nvrBranch
  where
    nvrBranch :: Package -> AnyBranch -> IO ()
    nvrBranch pkg br = do
      spec <- localBranchSpecFile pkg br
      case br of
        RelBranch rbr ->
          pkgNameVerRel' rbr spec
        OtherBranch _obr -> do
          sbr <- systemBranch
          pkgNameVerRel' sbr spec
        >>= putStrLn

commandCmd :: String -> Maybe BranchOpts -> [String] -> IO ()
commandCmd cs mbrnchopts =
  withPackageByBranches (Just True) Nothing mbrnchopts True AnyNumber cmdBranch
  where
    cmdBranch :: Package -> AnyBranch -> IO ()
    cmdBranch pkg _br = do
      curEnv <- getEnvironment
      let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
      (_,_,_,h) <- P.createProcess p
      void $ P.waitForProcess h

masterRenameCmd :: [String] -> IO ()
masterRenameCmd =
  withPackageByBranches (Just False) dirtyGit Nothing True ZeroOrOne masterRenameBranch
  where
  masterRenameBranch :: Package -> AnyBranch -> IO ()
  masterRenameBranch _pkg _br = do
    locals <- gitLines "branch" ["--format=%(refname:short)"]
    when ("rawhide" `notElem` locals) $ do
      git_ "fetch" ["--prune"]
      git_ "branch" ["--move", "master", "rawhide"]
      git_ "remote" ["set-head", "origin", "rawhide"]
      git_ "branch" ["--set-upstream-to", "origin/rawhide", "rawhide"]
