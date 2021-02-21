module Cmd.Install (
  installCmd,
  ) where

import Branches
import Common
import Common.System
import Package

-- FIXME package countdown
-- FIXME --ignore-uninstalled subpackages
-- FIXME --check any/all of package installed
installCmd :: Bool -> Maybe ForceShort -> [BCond] -> Bool -> [String] -> IO ()
installCmd recurse mforceshort bconds reinstall = do
  when (recurse && isJust mforceshort) $
    error' "cannot use --recurse and --shortcircuit"
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
                  Just pkgdir -> installCmd recurse mforceshort bconds reinstall [show br, pkgdir] >> putStrLn ""
            else error' $ "missing deps:\n" ++ unlines missingdeps
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
