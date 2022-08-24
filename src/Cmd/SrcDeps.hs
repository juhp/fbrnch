module Cmd.SrcDeps (
  srcDepsCmd
  )
where

import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order

import Branches
import Common
import Common.System
import Git
import Package

srcDepsCmd :: Bool -> (Branch,[String]) -> IO ()
srcDepsCmd rev (rbr,pkgs) = do
  when (null pkgs) $
    error' "please specify one or more package dirs"
  whenM isPkgGitRepo $
    error' "please run from the directory containing the dependency package set"
  listDirectory "." >>=
    filterM checkPackage . filter ((/= '.') . head) >>=
    createGraph3 [] [] False False (not rev) Nothing >>=
    createGraph2 [] False False True Nothing . dependencyNodes pkgs >>=
    sortGraph Combine
  where
    checkPackage :: FilePath -> IO Bool
    checkPackage p = do
      withExistingDirectory p $ do
        exists <- checkIfRemoteBranchExists (RelBranch rbr)
        if exists
          then do
          gitSwitchBranch (RelBranch rbr)
          isJust <$> maybeFindSpecfile
          else return False
