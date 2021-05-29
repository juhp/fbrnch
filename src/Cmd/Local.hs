module Cmd.Local (
  commandCmd,
  installDepsCmd,
  localCmd,
  nvrCmd,
  prepCmd,
  srpmCmd,
  renameMasterCmd
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import qualified System.Process as P
import qualified System.Process.Typed as TP

import Branches
import Common
import Common.System
import Git
import Package

localCmd :: Maybe ForceShort -> [BCond] -> (BranchesReq, [String]) -> IO ()
localCmd mforceshort bconds =
  withPackageByBranches Nothing Nothing ZeroOrOne localBuildPkg
  where
    localBuildPkg :: Package -> AnyBranch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- if isJust mforceshort
              then return []
              else builtRpms br spec
      buildRPMs False mforceshort bconds rpms br spec

installDepsCmd :: Maybe Branch -> [String] -> IO ()
installDepsCmd =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps False

srpmCmd :: Bool -> Maybe Branch -> [String] -> IO ()
srpmCmd force =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm' force (Just br) spec

-- FIXME option to clone package
prepCmd :: Maybe Branch -> [String] -> IO ()
prepCmd =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne prepPackage

nvrCmd :: (BranchesReq, [String]) -> IO ()
nvrCmd =
  withPackageByBranches Nothing Nothing AnyNumber nvrBranch
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

-- FIXME option to require spec file?
commandCmd :: Bool -> String -> (BranchesReq,[String]) -> IO ()
commandCmd ifoutput cs =
  withPackageByBranches (Just (not ifoutput)) Nothing AnyNumber cmdBranch
  where
    cmdBranch :: Package -> AnyBranch -> IO ()
    cmdBranch pkg br =
      ifM (doesFileExist "dead.package")
      (putStrLn "dead.package") $ do
      curEnv <- getEnvironment
      if ifoutput then do
        out <- TP.readProcessInterleaved_ $
                    TP.setEnv (("p",unPackage pkg):curEnv) $
                    TP.shell cs
        unless (B.null out) $ do
          putPkgAnyBrnchHdr pkg br
          B.putStr out
        else do
        let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
        (_,_,_,h) <- P.createProcess p
        void $ P.waitForProcess h

renameMasterCmd :: [String] -> IO ()
renameMasterCmd pkgs =
  withPackageByBranches (Just False) dirtyGit Zero renameMasterBranch (Branches [], pkgs)
  where
  renameMasterBranch :: Package -> AnyBranch -> IO ()
  renameMasterBranch _pkg _br = do
    locals <- gitLines "branch" ["--format=%(refname:short)"]
    when ("rawhide" `notElem` locals) $ do
      git_ "fetch" ["--prune"]
      git_ "branch" ["--move", "master", "rawhide"]
      git_ "remote" ["set-head", "origin", "rawhide"]
      git_ "branch" ["--set-upstream-to", "origin/rawhide", "rawhide"]
