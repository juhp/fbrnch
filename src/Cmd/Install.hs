module Cmd.Install (
  installCmd,
  notInstalledCmd
  ) where

import Data.RPM

import Branches
import Common
import Common.System
import Git
import Package
import Prompt

-- FIXME package countdown
-- FIXME --ignore-uninstalled subpackages
-- FIXME --skip-unavailable
-- FIXME --check any/all of package installed
installCmd :: Bool -> Bool -> Maybe ForceShort -> [BCond] -> Bool
           -> (Maybe Branch,[String]) -> IO ()
installCmd verbose recurse mforceshort bconds reinstall (mbr, pkgs) = do
  when (recurse && mforceshort == Just ShortCircuit) $
    error' "cannot use --recurse and --shortcircuit"
  when (length pkgs > 3) $
    putStrLn $ show (length pkgs) ++ " packages"
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne installPkg (mbr, pkgs)
  where
    installPkg :: Package -> AnyBranch -> IO ()
    installPkg pkg br = do
      whenJust mbr $ gitSwitchBranch . RelBranch
      spec <- localBranchSpecFile pkg br
      rpms <- builtRpms br spec
      -- removing arch
      let packages = map (readNVRA . takeFileName) rpms
      installed <- filterM rpmInstalled packages
      if isJust mforceshort || null installed || reinstall
        then doInstallPkg spec rpms installed
        else putStrLn $ unwords (map showNVRA installed) ++ " already installed!\n"
      where
        doInstallPkg spec rpms installed = do
          putStrLn $ "# " ++ takeBaseName (head rpms)
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
                  Nothing -> putStrLn $ dep ++ " not known"
                  Just pkgdir -> installCmd verbose recurse mforceshort bconds reinstall (mbr, [pkgdir]) >> putStrLn ""
              -- FIXME option to enable/disable installing missing deps
            else installDeps True spec
          wasbuilt <- buildRPMs (not verbose) mforceshort bconds rpms br spec
          unless (mforceshort == Just ShortCircuit) $
            if reinstall
            then do
              let reinstalls = filter (\ f -> readNVRA f `elem` installed) rpms
              unless (null reinstalls) $
                sudo_ "/usr/bin/dnf" $ "reinstall" : "-q" : "-y" : reinstalls
              let remaining = filterDebug $ rpms \\ reinstalls
              unless (null remaining) $
                sudo_ "/usr/bin/dnf" $ "install" : "-q" : "-y" : remaining
            else do
              ok <- cmdBool "sudo" $ "/usr/bin/dnf" : "install" : "-q" : "-y" : filterDebug rpms
              unless (ok || wasbuilt) $ do
                prompt_ "Press Enter to rebuild package"
                installCmd verbose recurse (Just ForceBuild) bconds reinstall (mbr, [unPackage pkg])

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
              repoquery rbr ["--qf=%{source_name}", "--whatprovides", p]

        filterDebug = filter (\p -> not (any (`isInfixOf` p) ["-debuginfo-", "-debugsource-"]))

notInstalledCmd :: (Maybe Branch,[String]) -> IO ()
notInstalledCmd =
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne notInstalledPkg
  where
    notInstalledPkg :: Package -> AnyBranch -> IO ()
    notInstalledPkg pkg br = do
      dead <- doesFileExist "dead.package"
      unless dead $ do
        spec <- findSpecfile
        rpms <- builtRpms br spec
        let packages = map readNVRA rpms
        installed <- filterM rpmInstalled packages
        when (null installed) $ do
          let pkgnames = map rpmName packages
          older <- filterM pkgInstalled pkgnames
          if null older
            then putStrLn $ unPackage pkg
            else putStrLn $ " " ++ unPackage pkg
