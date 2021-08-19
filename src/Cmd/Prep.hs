module Cmd.Prep (
  prepCmd
  ) where

import Branches
import Cmd.Clone
import Common
import Common.System
import Git
import InterleaveOutput (cmdSilent')
import Package

prepCmd :: Bool -> (Maybe Branch,[String]) -> IO ()
prepCmd clone (mbr,pkgs) = do
  when clone $
    cloneCmd mbr (ClonePkgs pkgs)
  withPackagesMaybeBranchNoHeadergit ZeroOrOne prepPackage (mbr,pkgs)
  where
    prepPackage :: Package -> AnyBranch -> IO ()
    prepPackage pkg br = do
      dead <- doesFileExist "dead.package"
      if dead
        then do
        when (length pkgs > 1)$
          putStr $ unPackage pkg ++ ": "
        putStrLn "dead.package"
        else do
        spec <- localBranchSpecFile pkg br
        unlessM (doesFileExist spec) $
          error' $ spec ++ " not found"
        cwd <- getCurrentDirectory
        void $ getSources spec
        gitDir <- isGitRepo
        let rpmdirs =
              [ "--define="++ mcr +-+ cwd | gitDir,
                mcr <- ["_builddir", "_sourcedir"]]
            args = rpmdirs ++ ["-bp", spec]
        case br of
          RelBranch rbr -> do
            nvr <- pkgNameVerRel' rbr spec
            -- newline avoids error starting on same line
            putStr $ "Prepping " ++ nvr ++ ": "
          _ -> return ()
        cmdSilent' "rpmbuild" args
        putStrLn "done"
