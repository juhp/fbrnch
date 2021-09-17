module Cmd.Mock
  ( mockCmd
  )
where

import Data.RPM

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME handle non-release branches (on-branch works)
mockCmd :: Bool -> Bool -> Bool -> Bool -> Maybe Branch
        -> (BranchesReq, [String]) -> IO ()
mockCmd dryrun noclean network noCleanAfter mroot (breq, ps) = do
  branches <-
    case breq of
      Branches [] ->
        if null ps
        then pure <$> getReleaseBranch
        else pure <$> systemBranch
      _ ->  listOfBranches False False breq
  when (null branches && length ps > 1 && isNothing mroot) $
    error' "Must specific branch or --root chroot"
  let packages = if null ps then ["."] else ps
  mapM_ (mockBuildPkgs (breq == Branches []) packages) branches
  where
    mockBuildPkgs :: Bool -> [String] -> Branch -> IO ()
    mockBuildPkgs noswitch pkgs br = do
      srpms <- mapM (prepSrpm (RelBranch br)) pkgs
      putStrLn ""
      rootBr <- maybe getReleaseBranch return mroot
      let resultdir =
            case srpms of
              [] -> error' "cannot build zero packages"
              [srpm] ->
                let verrel = showPkgVerRel . readNVRA $ srpm
                 in ["--resultdir=results" </> verrel]
              _ -> []
      let command = if length pkgs > 1 then "--chain" else "--rebuild"
      (if dryrun then cmdN else cmd_) "mock" $ [command, "--root", mockRoot rootBr] ++ ["--no-clean" | noclean] ++ ["--no-cleanup-after" | noCleanAfter] ++ ["--config-opts=cleanup_on_failure=False" | not noCleanAfter] ++ ["--enable-network" | network] ++ resultdir ++ srpms
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
