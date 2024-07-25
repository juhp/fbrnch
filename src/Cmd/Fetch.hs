module Cmd.Fetch (
    fetchPkgs
    )
where

import Branches
import Git
import Package

fetchPkgs :: [String] -> IO ()
fetchPkgs args =
  withPackagesByBranches
  (if length args > 1 then HeaderMust else HeaderMay)
  False
  dirtyGit
  Zero
  fetchPkg (Branches [],args)
  where
    fetchPkg :: Package -> AnyBranch -> IO ()
    fetchPkg _pkg _br =
      gitFetchSilent False
