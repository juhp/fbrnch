module Cmd.SrcDeps (
  srcDepsCmd,
  srcDeps
  )
where

import Distribution.RPM.Build.Graph

import Branches
import Common
import Common.System
import Git
import Package

srcDepsCmd :: Bool -> (Branch,[String]) -> IO ()
srcDepsCmd rev (rbr,pkgs) =
  srcDeps rev (rbr,pkgs) >>=
  mapM_ (putStrLn . unwords)

srcDeps :: Bool -> (Branch,[String]) -> IO [[String]]
srcDeps rev (rbr,pkgs) = do
  when (null pkgs) $
    error' "please specify one or more package dirs"
  whenM isPkgGitRepo $
    error' "please run from the directory containing the dependency package set"
  listDirectory "." >>=
    filterM checkPackage . filter ((/= '.') . head) >>=
    fmap (topsortGraph Combine) . depsGraphDeps rev [] False [] [] False Nothing pkgs
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
