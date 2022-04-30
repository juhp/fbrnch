module Cmd.Mock
  ( mockCmd,
    NoClean(..)
  )
where

import Data.RPM

import Branches
import Common
import Common.System
import Git
import Package

data NoClean = NoCleanBefore | NoCleanAfter | NoCleanAll
  deriving Eq

-- FIXME add repo/copr for build
-- FIXME handle non-release branches (on-branch works)
-- FIXME option for --shell without rebuild
mockCmd :: Bool -> Maybe NoClean -> Bool -> Bool -> Maybe Branch
        -> (BranchesReq, [String]) -> IO ()
mockCmd dryrun mnoclean network mockshell mroot (breq, ps) = do
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
          noclean = case mnoclean of
                      Nothing -> []
                      Just NoCleanBefore -> ["--no-clean"]
                      Just NoCleanAfter -> ["--no-cleanup-after"]
                      Just NoCleanAll -> ["--no-clean", "--no-cleanup-after"]
          mockopts_common c = [c, "--root", mockRoot rootBr] ++ noclean ++ ["--enable-network" | network]
          mockbuild_opts = mockopts_common command ++ ["--config-opts=cleanup_on_failure=False" | mnoclean `elem` [Nothing, Just NoCleanBefore]] ++ resultdir ++ srpms
          mockshell_opts = mockopts_common "--shell" ++ ["--no-clean" | "--no-clean" `notElem` noclean]
      if dryrun
        then do
        cmdN "mock" mockbuild_opts
        when mockshell $ cmdN "mock" mockshell_opts
        else do
        ok <- cmdBool "mock" mockbuild_opts
        when mockshell $ cmd_ "mock" mockshell_opts
        unless ok $ error' "mockbuild failed"
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
            generateSrpm (Just actualBr) spec
