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
import RpmBuild

data PrepPre = PrepClone | PrepPull
  deriving Eq

-- FIXME prompt for cloning
prepCmd :: Maybe PrepPre -> Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
prepCmd mpre verbose deps (mbr,pkgs) = do
  when (mpre == Just PrepClone) $
    cloneCmd (ClonePkgs (mbr, pkgs))
  withPackagesMaybeBranchNoHeadergit prepPackage (mbr,pkgs)
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
          error' $ spec +-+ "not found"
        getSourcesMacros spec
        when deps $
          installDeps False spec
        case br of
          RelBranch rbr -> do
            nvr <- pkgNameVerRel' rbr spec
            -- newline avoids error starting on same line
            putStr $ "Prepping" +-+ showNVR nvr ++ ": "
          _ -> return ()
        timeIO $
          (if verbose then cmdLog else cmdSilent') "rpmbuild" $ "-bp" : ["--nodeps" | not deps] ++ [spec]
        putStrLn "done"
