module Cmd.Local (
  commandCmd,
  countCmd,
  installDepsCmd,
  localCmd,
  moveArtifactsCmd,
  nvrCmd,
  renameRawhideCmd,
  srpmCmd,
  srpmSpecCmd
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Safe (headMay)
import System.Environment (getEnvironment)
import qualified System.Process as P
import qualified System.Process.Typed as TP
import System.Exit
import System.IO.Extra (withTempDir)

import Branches
import Common
import Common.System
import Git
import Package
import RpmBuild

localCmd :: Bool -> Bool -> Bool -> Maybe Natural -> Maybe ForceShort
         -> [BCond] -> (BranchesReq, [String]) -> IO ()
localCmd quiet debug allowhead mjobs mforceshort bconds (breq,pkgs) =
  if allowhead
  then if breq == Branches []
       then withPackagesMaybeBranch HeaderNone False dirtyGitHEAD localBuildPkgNoBranch (Nothing, pkgs)
       else error' "--detached-head only supported without specific branch(es)"
  else
  withPackagesByBranches HeaderNone False Nothing ZeroOrOne localBuildPkg (breq,pkgs)
  where
    localBuildPkg :: Package -> AnyBranch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- if isJust mforceshort
              then return []
              else builtRpms br spec
      -- FIXME backup BUILD tree to .prev
      void $ buildRPMs quiet debug True mjobs mforceshort bconds rpms br spec
      -- FIXME mark BUILD dir complete

    localBuildPkgNoBranch :: Package -> AnyBranch -> IO ()
    localBuildPkgNoBranch _pkg _ = do
      spec <- findSpecfile
      void $ buildRPMsNoBranch quiet debug True mjobs mforceshort bconds spec

installDepsCmd :: (Maybe Branch,[String]) -> IO ()
installDepsCmd =
  withPackagesMaybeBranch HeaderNone False Nothing installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps False

srpmCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
srpmCmd force =
  withPackagesMaybeBranchNoHeadergit srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm' force (Just br) spec

nvrCmd :: (BranchesReq, [String]) -> IO ()
nvrCmd =
  withPackagesByBranches HeaderNone False Nothing AnyNumber nvrBranch
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
        >>= putStrLn . showNVR

-- FIXME option to require spec file?
commandCmd :: Bool -> Bool -> Bool -> String -> (BranchesReq,[String])
           -> IO ()
commandCmd ifoutput compact continue cs =
  withPackagesByBranches (boolHeader (not ifoutput)) False Nothing AnyNumber cmdBranch
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
          whenJust (B.unsnoc out) $ \(bs,l) -> do
            if compact
              then putStr $ unPackage pkg ++ ": "
              else putPkgAnyBrnchHdr pkg br
            B.putStrLn $ if l == '\n' then bs else out
          return ret
          else do
          let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
          (_,_,_,h) <- P.createProcess p
          P.waitForProcess h
      unless (continue || ret == ExitSuccess)
        exitFailure

renameRawhideCmd :: [String] -> IO ()
renameRawhideCmd pkgs =
  withPackagesByBranches HeaderMay False dirtyGit Zero renameMasterBranch (Branches [], pkgs)
  where
  renameMasterBranch :: Package -> AnyBranch -> IO ()
  renameMasterBranch _pkg _br = renameMasterToRawhide

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

srpmSpecCmd :: Bool -> [FilePath] -> IO ()
srpmSpecCmd diff srpms =
  if diff then
    case srpms of
      [] -> error' "impossible happened: no srpms given"
      [srpm] -> do
        withTempDir $ \tempdir -> do
          spec <- getSrpmSpecfile False srpm tempdir
          cmd_ "diff" ["-u", tempdir </> spec, spec]
      [srpm1, srpm2] ->
        withTempDir $ \tempdir -> do
          spec1 <- getSrpmSpecfile True srpm1 tempdir
          spec2 <- getSrpmSpecfile True srpm2 tempdir
          withCurrentDirectory tempdir $
            void $ cmdBool "diff" ["-u", spec1, spec2]
      _ -> error' "too many srpm files"
    else
    forM_ srpms $ \srpm ->
    pipe_ ("rpm2cpio", [srpm]) ("cpio",["--extract", "--quiet", "--to-stdout", "*.spec"])
  where
    getSrpmSpecfile :: Bool -> FilePath -> FilePath -> IO FilePath
    getSrpmSpecfile sub srpm tempdir = do
      exists <- doesFileExist srpm
      if exists
        then do
        let subdir = if sub then takeBaseName srpm else ""
            dir = tempdir </> subdir
        ok <- pipeBool ("rpm2cpio", [srpm]) ("cpio", ["--extract", "--quiet", "--make-directories", "-D", dir , "--preserve-modification-time", "*.spec"])
        if ok
          then do
          mspec <- headMay <$> listDirectory dir
          case mspec of
            Nothing -> error' $ "no spec in" +-+ dir
            Just spec -> return $ subdir </> spec
          else error' "failed to extract spec file"
        else error' $ "no such file:" +-+ srpm

moveArtifactsCmd :: Bool -> [String] -> IO ()
moveArtifactsCmd remove pkgs =
  withPackagesByBranches HeaderMay False Nothing Zero moveArtifactsPkg (Branches [], pkgs)
  where
    moveArtifactsPkg :: Package -> AnyBranch -> IO ()
    moveArtifactsPkg pkg br = do
      cwd <- getCurrentDirectory
      whenJustM (rpmEval "%_rpmdir") $ \rpmdir ->
        unless (rpmdir == cwd) $ do
        -- FIXME hardcoding
        moveRPMS rpmdir "x86_64"
        moveRPMS rpmdir "noarch"
      ls <- listDirectory "."
      whenJustM (rpmEval "%_srcrpmdir") $ \srcrpmdir ->
        unless (srcrpmdir == cwd) $ do
        let srpms = filter ("src.rpm" `isExtensionOf`) ls
        forM_ srpms $ \srpm -> do
          exists <- doesFileExist $ srcrpmdir </> srpm
          if exists
            then if remove
                 then removeFile srpm
                 else putStrLn $ "duplicate:" +-+ srpm
            else do
            createDirectoryIfMissing False srcrpmdir
            renameFile srpm $ srcrpmdir </> srpm
      whenJustM (rpmEval "%_builddir") $ \builddir ->
        unless (builddir == cwd) $ do
        dirs <- filterM doesDirectoryExist ls
        spec <- localBranchSpecFile pkg br
        srcs <- map (takeWhile (not . isDigit) . takeBaseName) <$> cmdLines "spectool" ["-S", spec]
        let srctrees =
              case srcs of
                [] -> []
                (src:_) -> filter (src `isPrefixOf`) dirs
        createDirectoryIfMissing False builddir
        forM_ srctrees $ \tree -> do
          exists <- doesDirectoryExist $ builddir </> tree
          if exists
            then if remove
                 then removeDirectoryRecursive tree
                 else putStrLn $ "duplicate:" +-+ tree
            else renameDirectory tree $ builddir </> tree

    moveRPMS :: FilePath -> FilePath -> IO ()
    moveRPMS rpmdir archdir =
      whenM (doesDirectoryExist archdir) $ do
      haveRpmDir <- doesDirectoryExist (rpmdir </> archdir)
      if haveRpmDir
        then do
        rpms <- listDirectory archdir
        forM_ rpms $ \rpm -> do
          let file = archdir </> rpm
          exists <- doesFileExist $ rpmdir </> file
          if exists
            then if remove
                 then removeFile file
                 else putStrLn $ "duplicate:" +-+ file
            else renameFile file $ rpmdir </> file
        left <- listDirectory archdir
        when (null left) $
          removeDirectory archdir
        else do
        createDirectoryIfMissing False rpmdir
        renameDirectory archdir $ rpmdir </> archdir
