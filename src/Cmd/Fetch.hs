module Cmd.Fetch (
    fetchPkgs
    )
where

import Branches
import Common ((+-+))
import Common.System (error')
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
    fetchPkgLenient pkg _br = do
      haveGit <- isPkgGitRepo
      if haveGit
        then gitFetchSilent False
        else if lenient
             then putStrLn $ "ignoring" +-+ unPackage pkg
             else error' "not a dist-git dir"
