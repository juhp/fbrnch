{-# LANGUAGE CPP #-}

module Cmd.Install (
  installCmd,
  notInstalledCmd
  ) where

import Data.RPM
#if !MIN_VERSION_simple_cmd(0,2,7)
import System.Posix.User (getEffectiveUserID)
#endif
import SimplePrompt

import Branches
import Cmd.Merge
import Common
import Common.System
import Git
import Package
import Repoquery
import RpmBuild

-- FIXME --nobuild
-- FIXME --rpm to avoid dnf
-- FIXME --force removal of existing incompatible dependent packages
-- FIXME --subpackage to specify subpackage(s) to install/add
-- FIXME --ignore-uninstalled subpackages
-- FIXME --skip-unavailable
-- FIXME --check any/all of package installed
-- FIXME add --debug or respect --verbose for dnf commands
installCmd :: Bool -> Bool -> Maybe Branch -> Maybe ForceShort -> [BCond]
           -> Bool -> Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
installCmd verbose recurse mfrom mforceshort bconds reinstall nobuilddeps allsubpkgs (mbr, pkgs) = do
  when (recurse && isShortCircuit mforceshort) $
    error' "cannot use --recurse and --shortcircuit"
  withPackagesMaybeBranch (boolHeader (recurse || length pkgs > 1)) True Nothing installPkg (mbr, pkgs)
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
        -- removing arch
        let nvras = map readNVRA rpms
        already <- filterM nvraInstalled nvras
        if isJust mforceshort || null already || reinstall
          then doInstallPkg mforceshort spec rpms already
          else putStrLn $ unlines (map showNVRA already) ++
               "\nalready installed!\n"
      where
        doInstallPkg mforceshort' spec rpms already = do
          putStrLn $ (showNVR . dropArch . readNVRA) (head rpms)
          unless nobuilddeps $ do
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
                    Just pkgdir -> installCmd verbose recurse mfrom mforceshort bconds reinstall nobuilddeps allsubpkgs (mbr, [pkgdir]) >> putNewLn
                -- FIXME option to enable/disable installing missing deps
              else installDeps True spec
          wasbuilt <- buildRPMs (not verbose) False False mforceshort' bconds rpms br spec
          unless (isShortCircuit mforceshort') $ do
            toinstalls <-
              if allsubpkgs
              then return rpms
              else do
                ps <- filterM (pkgInstalled . rpmName . readNVRA) rpms
                return $ if null ps then rpms else ps
            if reinstall || mforceshort' == Just ForceBuild
              then do
              let reinstalls =
                    filter (\ f -> readNVRA f `elem` already) toinstalls
              unless (null reinstalls) $
                sudoLog "/usr/bin/dnf" $ "reinstall" : "-q" : "-y" : reinstalls
              let remaining = filterDebug $ toinstalls \\ reinstalls
              unless (null remaining) $
                sudoLog "/usr/bin/dnf" $ "install" : "-q" : "-y" : remaining
              else do
              let command = "/usr/bin/dnf" : "install" : "-q" : "-y" : filterDebug toinstalls
              cmdN "sudo" command
              ok <- cmdBool "sudo" command
              unless ok $
                if wasbuilt
                then error' $ "error from:" +-+ unwords command
                else do
                  prompt_ "Press Enter to rebuild package"
                  doInstallPkg (Just ForceBuild) spec rpms already

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

        filterDebug = filter (\p -> not (any (`isInfixOf` p) ["-debuginfo-", "-debugsource-"]))

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

nvraInstalled :: NVRA -> IO Bool
nvraInstalled rpm =
  cmdBool "rpm" ["--quiet", "-q", showNVRA rpm]

pkgInstalled :: String -> IO Bool
pkgInstalled pkg =
  cmdBool "rpm" ["--quiet", "-q", pkg]

#if !MIN_VERSION_simple_cmd(0,2,7)
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudoInternal cmdLog
  where
    sudoInternal :: (String -> [String] -> IO a) -> String -> [String] -> IO a
    sudoInternal exc c args = do
      uid <- getEffectiveUserID
      sd <- if uid == 0
        then return Nothing
        else findExecutable "sudo"
      let noSudo = isNothing sd
      when (uid /= 0 && noSudo) $
        warning "'sudo' not found"
      exc (fromMaybe c sd) (if noSudo then args else c:args)
#endif
