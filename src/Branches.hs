module Branches (
  packageBranches,
  packageBranched,
  packagePagureBranched,
  getCurrentBranch,
  module Distribution.Fedora.Branch
) where

import Common

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git

import Pagure

getCurrentBranch :: IO Branch
getCurrentBranch = do
  active <- getFedoraBranches
  readActiveBranch' active <$> git "rev-parse" ["--abbrev-ref", "HEAD"]

activeBranches :: [Branch] -> [String] -> [Branch]
activeBranches active =
  -- newest branch first
  {- HLINT ignore "Avoid reverse"-} -- reverse . sort is fast but not stabilizing
  reverse . sort . mapMaybe (readActiveBranch active)

packageBranches :: IO [Branch]
packageBranches = do
  active <- getFedoraBranches
  activeBranches active <$>
    cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-1)"]

packageBranched :: IO [Branch]
packageBranched = do
  active <- getFedoraBranched
  activeBranches active <$>
    cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-1)"]

packagePagureBranched :: String -> IO [Branch]
packagePagureBranched pkg = do
  current <- getFedoraBranched
  res <- pagureListGitBranches srcfpo ("rpms/" ++ pkg)
  return $ either error' (activeBranches current) res
