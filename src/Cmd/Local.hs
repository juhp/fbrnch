module Cmd.Local (
  commandCmd,
  countCmd,
  installDepsCmd,
  localCmd,
  nvrCmd,
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
import Package

localCmd :: Bool -> Maybe ForceShort -> [BCond] -> (BranchesReq, [String])
         -> IO ()
localCmd quiet mforceshort bconds =
  withPackageByBranches Nothing Nothing ZeroOrOne localBuildPkg
  where
    localBuildPkg :: Package -> AnyBranch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- if isJust mforceshort
              then return []
              else builtRpms br spec
      void $ buildRPMs quiet mforceshort bconds rpms br spec

installDepsCmd :: (Maybe Branch,[String]) -> IO ()
installDepsCmd =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps False

srpmCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
srpmCmd force =
  withPackagesMaybeBranchNoHeadergit ZeroOrOne srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm' force (Just br) spec

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
    -- FIXME dangling warning in current output:
      -- From ssh://pkgs.fedoraproject.org/rpms/hedgewars
      --  - [deleted]         (none)     -> origin/master
      --    (refs/remotes/origin/HEAD has become dangling)
      -- Branch 'rawhide' set up to track remote branch 'rawhide' from 'origin'.
    -- compare commands with github rename
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
