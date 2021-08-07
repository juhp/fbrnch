module Cmd.Local (
  commandCmd,
  countCmd,
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
import System.Exit

import Branches
import Common
import Common.System
import Git
import InterleaveOutput (cmdSilent')
import Package

-- FIXME generate build.log files
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
      void $ buildRPMs False mforceshort bconds rpms br spec

installDepsCmd :: (Maybe Branch,[String]) -> IO ()
installDepsCmd =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps False

srpmCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
srpmCmd force =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm' force (Just br) spec

-- FIXME option to clone package
prepCmd :: (Maybe Branch,[String]) -> IO ()
prepCmd (mbr,pkgs) =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne prepPackage (mbr,pkgs)
  where
    prepPackage :: Package -> AnyBranch -> IO ()
    prepPackage pkg br = do
      dead <- doesFileExist "dead.package"
      if dead
        then do
        when (length pkgs > 1)$
          putStr $ unPackage pkg ++ ": "
        putStrLn "dead.package"
        else do
        spec <- localBranchSpecFile pkg br
        unlessM (doesFileExist spec) $
          error' $ spec ++ " not found"
        cwd <- getCurrentDirectory
        void $ getSources spec
        gitDir <- isGitRepo
        let rpmdirs =
              [ "--define="++ mcr +-+ cwd | gitDir,
                mcr <- ["_builddir", "_sourcedir"]]
            args = rpmdirs ++ ["-bp", spec]
        case br of
          RelBranch rbr -> do
            nvr <- pkgNameVerRel' rbr spec
            -- newline avoids error starting on same line
            putStr $ "Prepping " ++ nvr ++ ": "
          _ -> return ()
        cmdSilent' "rpmbuild" args
        putStrLn "done"

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
commandCmd :: Bool -> Bool -> Bool -> String -> (BranchesReq,[String])
           -> IO ()
commandCmd ifoutput compact continue cs =
  withPackageByBranches (Just (not ifoutput)) Nothing AnyNumber cmdBranch
  where
    cmdBranch :: Package -> AnyBranch -> IO ()
    cmdBranch pkg br =
      unlessM (doesFileExist "dead.package") $ do
      curEnv <- getEnvironment
      ret <-
        if ifoutput then do
          (ret,out) <- TP.readProcessInterleaved $
                       TP.setEnv (("p",unPackage pkg):curEnv) $
                       TP.shell cs
          unless (B.null out) $ do
            if compact
              then putStr $ unPackage pkg ++ ": "
              else putPkgAnyBrnchHdr pkg br
            B.putStr out
          return ret
          else do
          let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
          (_,_,_,h) <- P.createProcess p
          P.waitForProcess h
      unless (continue || ret == ExitSuccess)
        exitFailure

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

countCmd :: (Maybe Branch,[String]) -> IO ()
countCmd (mbr,pkgs) =
  foldM countPkg 0 pkgs >>= print
  where
    -- FIXME dead.package?
    countPkg :: Int -> String ->  IO Int
    countPkg n path =
      withExistingDirectory path $ do
      whenJust mbr $ gitSwitchBranch . RelBranch
      mspec <- if ".spec" `isExtensionOf` path
               then return $ Just $ takeFileName path
               else maybeFindSpecfile
      case mspec of
        Just spec -> do
          exists <- doesFileExist spec
          return $ n + if exists then 1 else 0
        Nothing -> return n
