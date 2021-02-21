module Cmd.Local (
  commandCmd,
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
import Git
import Package

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
    dummy _pkg br =
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
