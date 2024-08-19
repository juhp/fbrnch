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
-- FIXME to skip prep or deps (eg in broken toolbox when deps can't install;)
prepCmd :: Maybe PrepPre -> Bool -> Bool -> Bool -> (Maybe Branch,[String])
        -> IO ()
prepCmd mpre verbose deps allowhead (mbr,pkgs) = do
  when (mpre == Just PrepClone) $
    cloneCmd mbr (ClonePkgs pkgs)
  withPackagesMaybeBranch HeaderNone False (if allowhead then dirtyGitHEAD else Nothing) prepPackage (mbr,pkgs)
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
        spec <- if allowhead then findSpecfile else localBranchSpecFile pkg br
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
        sourcediropt <- do
          distgit <- isGitRepo
          if distgit
            then sourceDirCwdOpt
            else return []
        timeIO $
          (if verbose then cmdLog else cmdSilent') "rpmbuild" $ "-bp" : ["--nodeps" | not deps] ++ sourcediropt ++ [spec]
        putStrLn "done"
