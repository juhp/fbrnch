module Cmd.Prep (
  prepCmd,
  PrepPre(..)
  ) where

import Branches
import Cmd.Clone
import Common
import Common.System
import Git
import InterleaveOutput (cmdSilent')
import Package

data PrepPre = PrepClone | PrepPull
  deriving Eq

prepCmd :: Maybe PrepPre -> Bool -> (Maybe Branch,[String]) -> IO ()
prepCmd mpre verbose (mbr,pkgs) = do
  when (mpre == Just PrepClone) $
    cloneCmd (ClonePkgs (mbr, pkgs))
  withPackagesMaybeBranchNoHeadergit ZeroOrOne prepPackage (mbr,pkgs)
  where
    prepPackage :: Package -> AnyBranch -> IO ()
    prepPackage pkg br = do
      when (mpre == Just PrepPull) $
        git_ "pull" []
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
        void $ getSources spec
        installMissingMacros spec
        case br of
          RelBranch rbr -> do
            nvr <- pkgNameVerRel' rbr spec
            -- newline avoids error starting on same line
            putStr $ "Prepping " ++ nvr ++ ": "
          _ -> return ()
        timeIO $
          (if verbose then cmdLog else cmdSilent') "rpmbuild" ["-bp", spec]
        putStrLn "done"
