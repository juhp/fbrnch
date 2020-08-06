module Cmd.Local (
  installCmd,
  localCmd,
  mockCmd,
  prepCmd,
  sortCmd,
  RpmWith(..),
  srpmCmd
  ) where

import Distribution.RPM.Build.Order (dependencySortRpmOpts)

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME package countdown
-- FIXME --ignore-uninstalled subpackages
installCmd :: Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
installCmd force reinstall (mbr,pkgs) = do
  withPackageByBranches False Nothing installPkg (maybeBranches mbr,pkgs)
  where
    installPkg :: Package -> Branch -> IO ()
    installPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- builtRpms br spec
      when (null rpms) $
        error' $ spec ++ " does not seem to create any rpms"
      -- removing arch
      let packages = map takeNVRName rpms
      installed <- filterM pkgInstalled packages
      if force || null installed || (reinstall && length installed /= length packages)
        then doInstallPkg spec rpms installed
        else putStrLn $ unwords installed +-+ "already installed!\n"
      where
        doInstallPkg spec rpms installed = do
          let baserpm = head rpms
          putStrLn $ takeBaseName baserpm ++ "\n"
          specNewer <-
            ifM (notM (doesFileExist baserpm))
            (return True) $
            do specTime <- getModificationTime spec
               rpmTime <- getModificationTime $ head rpms
               return $ specTime > rpmTime
          allrpmsbuilt <- and <$> mapM doesFileExist rpms
          when (force || specNewer || not allrpmsbuilt) $ do
            buildRPMs True False br spec
            putStrLn ""
          if reinstall then do
            let reinstalls = filter (\ f -> takeNVRName f `elem` installed) rpms
            unless (null reinstalls) $
              sudo_ "/usr/bin/dnf" $ "reinstall" : "-q" : "-y" : reinstalls
            let remaining = filterDebug $ rpms \\ reinstalls
            unless (null remaining) $
              sudo_ "/usr/bin/dnf" $ "install" : "-q" : "-y" : remaining
            else sudo_ "/usr/bin/dnf" $ "install" : "-q" : "-y" : filterDebug rpms

        takeNVRName = takeBaseName . takeBaseName

        filterDebug = filter (\p -> not (any (`isInfixOf` p) ["-debuginfo-", "-debugsource-"]))

localCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
localCmd shortcircuit (mbr,pkgs) =
  withPackageByBranches False Nothing localBuildPkg (maybeBranches mbr,pkgs)
  where
    localBuildPkg :: Package -> Branch -> IO ()
    localBuildPkg pkg br =
      localBranchSpecFile pkg br >>=
      buildRPMs False shortcircuit br

srpmCmd :: (Maybe Branch,[String]) -> IO ()
srpmCmd (mbr,pkgs) =
  withPackageByBranches False Nothing srpmBuildPkg (maybeBranches mbr,pkgs)

srpmBuildPkg :: Package -> Branch -> IO ()
srpmBuildPkg pkg br = do
  spec <- localBranchSpecFile pkg br
  void $ getSources spec
  void $ generateSrpm (Just br) spec

data RpmWith = RpmWith String | RpmWithout String

sortCmd :: Maybe RpmWith -> (Maybe Branch,[String]) -> IO ()
sortCmd _ (_,[]) = return ()
sortCmd mrpmwith (mbr,pkgs) = do
  withPackageByBranches False Nothing dummy (maybeBranches mbr,pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  packages <- dependencySortRpmOpts rpmopts $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br = gitSwitchBranch br

    toRpmOption :: RpmWith -> [String]
    toRpmOption (RpmWith opt) = ["--with=" ++ opt]
    toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

prepCmd :: (Maybe Branch,[String]) -> IO ()
prepCmd (mbr,pkgs) =
  withPackageByBranches False Nothing prepPackage (maybeBranches mbr,pkgs)

mockCmd :: Maybe Branch -> (Maybe Branch,[String]) -> IO ()
mockCmd mroot (mbr,pkgs) =
  withPackageByBranches True Nothing mockBuildPkg (maybeBranches mbr,pkgs)
  where
    mockBuildPkg :: Package -> Branch -> IO ()
    mockBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      whenM isPkgGitRepo $ gitSwitchBranch br
      void $ getSources spec
      srpm <- generateSrpm (Just br) spec
      let pkgname = unPackage pkg
          mverrel = stripInfix "-" $ removePrefix (pkgname ++ "-") $ takeBaseName (takeBaseName srpm)
          verrel = maybe "" (uncurry (</>)) mverrel
      let resultsdir = "results_" ++ pkgname </> verrel
          rootBr = fromMaybe br mroot
      cmd_ "mock" ["--root", mockConfig rootBr, "--resultdir=" ++ resultsdir, srpm]
