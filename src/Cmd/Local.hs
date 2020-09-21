module Cmd.Local (
  commandCmd,
  installCmd,
  installDepsCmd,
  localCmd,
  mockCmd,
  nvrCmd,
  prepCmd,
  sortCmd,
  RpmWith(..),
  srpmCmd
  ) where

import Distribution.RPM.Build.Order (dependencySortRpmOpts)
import qualified System.Process as P
import System.Environment

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME package countdown
-- FIXME --ignore-uninstalled subpackages
installCmd :: Maybe ForceShort -> Bool -> [String] -> IO ()
installCmd mforceshort reinstall =
  withPackageByBranches Nothing Nothing Nothing oneBranch installPkg
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
          buildRPMs True mforceshort rpms br spec
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

        filterDebug = filter (\p -> not (any (`isInfixOf` p) ["-debuginfo-", "-debugsource-"]))

takeNVRName :: FilePath -> String
takeNVRName = takeBaseName . takeBaseName

localCmd :: Maybe ForceShort -> [String] -> IO ()
localCmd mforceshort =
  withPackageByBranches Nothing Nothing Nothing zeroOneBranches localBuildPkg
  where
    localBuildPkg :: Package -> AnyBranch -> IO ()
    localBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      rpms <- builtRpms br spec
      buildRPMs False mforceshort rpms br spec

-- FIXME single branch
installDepsCmd :: [String] -> IO ()
installDepsCmd =
  withPackageByBranches Nothing Nothing Nothing zeroOneBranches installDepsPkg
  where
    installDepsPkg :: Package -> AnyBranch -> IO ()
    installDepsPkg pkg br =
      localBranchSpecFile pkg br >>= installDeps

-- FIXME single branch
srpmCmd :: [String] -> IO ()
srpmCmd =
  withPackageByBranches Nothing Nothing Nothing zeroOneBranches srpmBuildPkg
  where
    srpmBuildPkg :: Package -> AnyBranch -> IO ()
    srpmBuildPkg pkg br = do
      spec <- localBranchSpecFile pkg br
      void $ generateSrpm (Just br) spec

data RpmWith = RpmWith String | RpmWithout String

sortCmd :: Maybe RpmWith -> [String] -> IO ()
sortCmd _ [] = return ()
sortCmd mrpmwith args = do
  (brs,pkgs) <- splitBranchesPkgs False Nothing args
  withPackageByBranches' Nothing Nothing Nothing oneBranch dummy (brs,pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  packages <- dependencySortRpmOpts rpmopts $ reverse pkgs
  putStrLn $ unwords packages
  where
    dummy _ br = gitSwitchBranch br

    toRpmOption :: RpmWith -> [String]
    toRpmOption (RpmWith opt) = ["--with=" ++ opt]
    toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

prepCmd :: [String] -> IO ()
prepCmd =
  withPackageByBranches Nothing Nothing Nothing zeroOneBranches prepPackage

mockCmd :: Maybe Branch -> [String] -> IO ()
mockCmd mroot args = do
  (brs,pkgs) <- splitBranchesPkgs True Nothing args
  if null pkgs
    then do
    unlessM isPkgGitRepo $
      error' "Please specify at least one package"
    else do
    whenM isPkgGitRepo $
      error' "Cannot build multiple packages inside a package dir"
  when (null brs && length pkgs > 1 && isNothing mroot) $
    error' "Must specific branch or --root chroot"
  branches <-
    if null brs
    then if null pkgs then pure <$> getReleaseBranch
         else pure <$> systemBranch
    else map onlyRelBranch <$> listOfBranches False False Nothing brs
  let packages = if null pkgs then ["."] else pkgs
  mapM_ (mockBuildPkgs (null brs) packages) branches
  where
    mockBuildPkgs :: Bool -> [String] -> Branch -> IO ()
    mockBuildPkgs noswitch pkgs br = do
      srpms <- mapM (prepSrpm (RelBranch br)) pkgs
      putStrLn ""
      rootBr <- maybe getReleaseBranch return mroot
      let resultdir =
            case pkgs of
              [] -> error' "cannot build zero packages"
              [pkg] ->
                let mverrel = stripInfix "-" $ removePrefix (pkg ++ "-") $ takeNVRName(head srpms)
                    verrel = maybe "" (uncurry (</>)) mverrel
                in ["--resultdir=results_" ++ pkg </> verrel]
              _ -> []
      let command = if length pkgs > 1 then "--chain" else "--rebuild"
      cmd_ "mock" $ [command, "--root", mockConfig rootBr] ++ resultdir ++ srpms
      where
        prepSrpm :: AnyBranch -> FilePath -> IO FilePath
        prepSrpm rbr pkgdir =
          withExistingDirectory pkgdir $ do
          pkg <- getPackageName pkgdir
          putPkgHdr pkg
          actualBr <-
            ifM (notM isPkgGitRepo)
            (return rbr)
            (if noswitch then gitCurrentBranch
             else gitSwitchBranch rbr >> return rbr)
          spec <- findSpecfile
          (pkgdir </>) . takeFileName <$> generateSrpm (Just actualBr) spec

nvrCmd :: Maybe BranchOpts -> [String] -> IO ()
nvrCmd mbrnchopts =
  withPackageByBranches Nothing Nothing mbrnchopts Nothing nvrBranch
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

commandCmd :: String -> Maybe BranchOpts -> [String] -> IO ()
commandCmd cs mbrnchopts =
  withPackageByBranches (Just True) Nothing mbrnchopts Nothing cmdBranch
  where
    cmdBranch :: Package -> AnyBranch -> IO ()
    cmdBranch pkg _br = do
      curEnv <- getEnvironment
      let p = (P.shell cs) { P.env = Just (("p",unPackage pkg):curEnv) }
      (_,_,_,h) <- P.createProcess p
      void $ P.waitForProcess h
