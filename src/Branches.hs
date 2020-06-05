module Branches (
  fedoraBranches,
  fedoraBranchesNoMaster,
  localBranches,
  pagurePkgBranches,
  module Distribution.Fedora.Branch
) where

import Common

import Distribution.Fedora.Branch
import SimpleCmd

import Pagure

activeBranches :: [Branch] -> [String] -> [Branch]
activeBranches active =
  -- newest branch first
  {- HLINT ignore "Avoid reverse"-} -- no longer as of hlint-3.0
  reverse . sort . mapMaybe (readActiveBranch active)

fedoraBranches :: IO [String] -> IO [Branch]
fedoraBranches mthd = do
  active <- getFedoraBranches
  activeBranches active <$> mthd

fedoraBranchesNoMaster :: IO [String] -> IO [Branch]
fedoraBranchesNoMaster mthd = do
  active <- getFedoraBranched
  activeBranches active <$> mthd

localBranches :: IO [String]
localBranches =
  cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-1)"]

pagurePkgBranches :: String -> IO [String]
pagurePkgBranches pkg = do
  res <- pagureListGitBranches srcfpo ("rpms/" ++ pkg)
  return $ either error' id res
