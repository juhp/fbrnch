{-# LANGUAGE CPP #-}

module Cmd.Install (
  installCmd,
  notInstalledCmd,
  Select(..),
  unInstallCmd
  ) where

import Data.RPM
import Safe (headMay)
import SelectRPMs

import Branches
import Cmd.Merge
import Common
import Common.System
import Git
import Package
import Repoquery
import RpmBuild

-- FIXME --rpm to avoid dnf
-- FIXME --subpackage to specify subpackage(s) to install/add
-- FIXME --exclude to specify subpackage(s) not to install
-- FIXME --ignore-uninstalled subpackages
-- FIXME --skip-unavailable
-- FIXME --check any/all of package installed
-- FIXME add --debug or respect --verbose for dnf commands
-- FIXME handle subpackage renames (eg ghc-rpm-macros-no-prof to ghc-rpm-macros-quick)
-- FIXME allow building an srpm
installCmd :: Bool -> Bool -> Maybe Branch -> Maybe Natural
           -> Maybe ForceShort -> [BCond] -> Maybe PkgMgr -> Bool -> Bool
           -> Bool -> Bool -> Yes -> Select -> Maybe ExistingStrategy
           -> (Maybe Branch,[String]) -> IO ()
installCmd quiet recurse mfrom mjobs mforceshort bconds mmgr reinstall allowerasing nobuild nobuilddeps yes select mexisting (mbr, pkgs) = do
  when (recurse && isShortCircuit mforceshort) $
    error' "cannot use --recurse and --shortcircuit"
  withPackagesMaybeBranch (boolHeader (recurse || length pkgs > 1)) True Nothing installPkg (mbr, filter (/= ":") pkgs)
  where
    installPkg :: Package -> AnyBranch -> IO ()
    installPkg pkg br = do
      whenJust mbr $ gitSwitchBranch . RelBranch
      dead <- doesFileExist "dead.package"
      if dead
        then putStrLn "dead package"
        else do
        spec <-
          if isNothing mfrom
          then localBranchSpecFile pkg br
          else do
            mergeCmd False False True Nothing False mfrom (Branches [onlyRelBranch br], ["."])
            localBranchSpecFile pkg br
        rpms <- builtRpms br spec
        let nvras = map readNVRA rpms
        -- FIXME can this be removed now?
        already <- filterM nvraInstalled nvras
        if isJust mforceshort || null already || reinstall || select /= selectDefault
          then doInstallRPMs mforceshort spec rpms
          else putStrLn $ unlines (map showNVRA already) ++
               "\nalready installed!\n"
      where
        doInstallRPMs mforceshort' spec rpms = do
          -- FIXME show source NVR (eg not pandoc-common)
          whenJust (headMay rpms) $
            putStrLn . showNVR . dropArch . readNVRA
          unless (nobuilddeps || nobuild) $ do
            missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
            unless (null missingdeps) $
              if recurse
              then do
                -- srcs <- nub <$> mapM (derefSrcPkg topdir dist True) hmissing
                rbr <- anyBranchToRelease br
                forM_ missingdeps $ \ dep -> do
                  -- FIXME check not metadep with parens
                  mpkgdir <- lookForPkgDir rbr ".." dep
                  case mpkgdir of
                    Nothing -> putStrLn $ dep +-+ "not known"
                    Just pkgdir -> installCmd quiet recurse mfrom mjobs mforceshort bconds mmgr reinstall allowerasing nobuild nobuilddeps yes select mexisting (mbr, [pkgdir]) >> putNewLn
                -- FIXME option to enable/disable installing missing deps
                -- FIXME --skip-missing-deps or prompt
              else installDeps True spec
          -- FIXME unused
          _wasbuilt <-
            if nobuild
            then return True
            else buildRPMs quiet False False mjobs mforceshort' bconds rpms br spec
          unless (isShortCircuit mforceshort') $ do
            let nvras = rpmsToNVRAs rpms
                -- FIXME: prefix = fromMaybe (nvrName nvr) mprefix
            decided <- decideRPMs yes False mexisting select (unPackage pkg) nvras
            -- FIXME dryrun and debug
            -- FIXME return Bool?
            installRPMsAllowErasing False False mmgr allowerasing yes $ groupOnArch "RPMS" decided

        lookForPkgDir :: Branch -> FilePath -> String -> IO (Maybe FilePath)
        lookForPkgDir rbr topdir p = do
          mdir <- checkForPkgDir p
          case mdir of
            Just dir -> return $ Just dir
            Nothing ->
              if '-' `elem` p then do
                -- FIXME check provides p
                mdir' <- checkForPkgDir (init (dropWhileEnd (/= '-') p))
                case mdir' of
                  Just dir' -> return $ Just dir'
                  Nothing -> repoquerySrc >>= checkForPkgDir
              else repoquerySrc >>= checkForPkgDir
          where
            checkForPkgDir :: FilePath -> IO (Maybe FilePath)
            checkForPkgDir p' = do
              let pdir = topdir </> p'
              exists <- doesDirectoryExist pdir
              return $ if exists
                       then Just pdir
                       else Nothing

            repoquerySrc = do
              putStrLn $ "Repoquerying" +-+ p
              sysbr <- systemBranch
              repoquery sysbr rbr ["--qf=%{source_name}", "--whatprovides", p]

notInstalledCmd :: (Maybe Branch,[String]) -> IO ()
notInstalledCmd =
  withPackagesMaybeBranchNoHeadergit notInstalledPkg
  where
    notInstalledPkg :: Package -> AnyBranch -> IO ()
    notInstalledPkg pkg br = do
      dead <- doesFileExist "dead.package"
      unless dead $ do
        spec <- findSpecfile
        rpms <- builtRpms br spec
        let nvras = map readNVRA rpms
        installed <- filterM nvraInstalled nvras
        when (null installed) $ do
          let pkgnames = map rpmName nvras
          older <- filterM pkgInstalled pkgnames
          if null older
            then putStrLn $ unPackage pkg
            else putStrLn $ " " ++ unPackage pkg

pkgInstalled :: String -> IO Bool
pkgInstalled pkg =
  cmdBool "rpm" ["--quiet", "-q", pkg]

unInstallCmd :: Yes -> (Maybe Branch,[String]) -> IO ()
unInstallCmd yes (mbr, pkgs) = do
  withPackagesMaybeBranch (boolHeader (length pkgs > 1)) True Nothing unInstallPkg (mbr, pkgs)
  where
    unInstallPkg :: Package -> AnyBranch -> IO ()
    unInstallPkg pkg br = do
      whenJust mbr $ gitSwitchBranch . RelBranch
      dead <- doesFileExist "dead.package"
      if dead
        then putStrLn "dead package"
        else do
        spec <- localBranchSpecFile pkg br
        built <- builtRpms br spec
        let rpms = map (rpmName . readNVRA) built
        -- FIXME can this be removed now?
        installed <- filterM (\p -> cmdBool "rpm" ["--quiet", "-q", p]) rpms
        unless (null installed) $ do
          whenJust (headMay built) $
            putStrLn . showNVR . dropArch . readNVRA
          unInstallRPMs False installed

    unInstallRPMs :: Bool -> [String] -> IO ()
    unInstallRPMs debug rpms =
      unless (null rpms) $ do
      mgr <- do
        ostree <- doesDirectoryExist "/sysroot/ostree"
        if ostree
          then return OSTREE
          else do
          mdnf5 <- findExecutable "dnf5"
          return $ maybe DNF3 (const DNF5) mdnf5
      let pkgmgr =
            case mgr of
              DNF3 -> "dnf-3"
              DNF5 -> "dnf5"
              RPM -> "rpm"
              OSTREE -> "rpm-ostree"
          com = unInstallCommand mgr
        in
        do
          when debug $ mapM_ putStrLn rpms
          (case mgr of
            OSTREE -> cmd_
            _ -> if debug then sudoLog else sudo_) pkgmgr $
            com ++ rpms ++ ["--assumeyes" | yes == Yes && mgr `elem` [DNF3,DNF5]]

    unInstallCommand :: PkgMgr -> [String]
    unInstallCommand mgr =
      case mgr of
        DNF3 -> ["remove"]
        DNF5 -> ["remove"]
        RPM -> ["-e"]
        OSTREE -> ["uninstall"]

#if !MIN_VERSION_simple_cmd(0,2,7)
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudo_
#endif
