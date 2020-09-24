module Cmd.Mock
  ( mockCmd,
  )
where

import Branches
import Common
import Common.System
import Git
import Package

mockCmd :: Maybe Branch -> [String] -> IO ()
mockCmd mroot args = do
  (brs, pkgs) <- splitBranchesPkgs True Nothing args
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
      then
        if null pkgs
          then pure <$> getReleaseBranch
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
                let mverrel = stripInfix "-" $ removePrefix (pkg ++ "-") $ takeNVRName (head srpms)
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
              ifM
                (notM isPkgGitRepo)
                (return rbr)
                ( if noswitch
                    then gitCurrentBranch
                    else gitSwitchBranch rbr >> return rbr
                )
            spec <- findSpecfile
            (pkgdir </>) . takeFileName <$> generateSrpm (Just actualBr) spec
