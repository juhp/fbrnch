module Cmd.Fetch (
    fetchPkgs
    )
where

import Branches
import Common (when, (+-+))
import Git
import Package

fetchPkgs :: Bool -> [String] -> IO ()
fetchPkgs lenient args =
  withPackagesByBranches
  (if length args > 1 then HeaderMust else HeaderMay)
  False
  (if lenient then Nothing else dirtyGitFetch)
  Zero
  fetchPkgLenient
  (Branches [], args)
  where
    fetchPkgLenient :: Package -> AnyBranch -> IO ()
    fetchPkgLenient pkg _br =
      when lenient $ do
      haveGit <- isPkgGitRepo
      if haveGit
        then gitFetchSilent False
        else putStrLn $ "ignoring" +-+ unPackage pkg
