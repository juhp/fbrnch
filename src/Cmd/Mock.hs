module Cmd.Mock
  ( mockCmd,
  )
where

import Branches
import Common
import Common.System
import Git
import Package

mockCmd :: Bool -> Bool -> Bool -> Maybe Branch -> [String] -> IO ()
mockCmd noclean network noCleanAfter mroot args = do
  (brs, pkgs) <- splitBranchesPkgs True Nothing args
  unless (null pkgs) $
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
              [_] ->
                let verrel = joinPath $ takeEnd 2 $ splitOn "-" $ takeNVRName (head srpms)
                 in ["--resultdir=results" </> verrel]
              _ -> []
      let command = if length pkgs > 1 then "--chain" else "--rebuild"
      cmd_ "mock" $ [command, "--root", mockConfig rootBr] ++ ["--no-clean" | noclean] ++ ["--no-clean-after" | noCleanAfter] ++ ["--enable-network" | network] ++ resultdir ++ srpms
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
