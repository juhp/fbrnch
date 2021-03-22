module Cmd.Local (
  commandCmd,
  installDepsCmd,
  localCmd,
  nvrCmd,
  prepCmd,
  sortCmd,
  RpmWith(..),
  srpmCmd,
  renameMasterCmd
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Distribution.RPM.Build.Order (dependencySortRpmOpts)
import System.Environment
import qualified System.Process as P
import qualified System.Process.Typed as TP

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
      rpms <- if isJust mforceshort
              then return []
              else builtRpms br spec
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

commandCmd :: Bool -> String -> Maybe BranchOpts -> [String] -> IO ()
commandCmd ifoutput cs mbrnchopts =
  withPackageByBranches (Just (not ifoutput)) Nothing mbrnchopts True AnyNumber cmdBranch
  where
    cmdBranch :: Package -> AnyBranch -> IO ()
    cmdBranch pkg br = do
      curEnv <- getEnvironment
      if ifoutput then do
        out <- TP.readProcessInterleaved_ $
                    TP.setEnv (("p",unPackage pkg):curEnv) $
                    TP.shell cs
        unless (B.null out) $ do
          putPkgAnyBrnchHdr pkg br
          B.putStrLn out
        else do
        let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
        (_,_,_,h) <- P.createProcess p
        void $ P.waitForProcess h

renameMasterCmd :: [String] -> IO ()
renameMasterCmd =
  withPackageByBranches (Just False) dirtyGit Nothing True ZeroOrOne renameMasterBranch
  where
  renameMasterBranch :: Package -> AnyBranch -> IO ()
  renameMasterBranch _pkg _br = do
    locals <- gitLines "branch" ["--format=%(refname:short)"]
    when ("rawhide" `notElem` locals) $ do
      git_ "fetch" ["--prune"]
      git_ "branch" ["--move", "master", "rawhide"]
      git_ "remote" ["set-head", "origin", "rawhide"]
      git_ "branch" ["--set-upstream-to", "origin/rawhide", "rawhide"]
