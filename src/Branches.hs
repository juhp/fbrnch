module Branches (
  fedoraBranches,
  fedoraBranchesNoMaster,
  localBranches,
  pagurePkgBranches,
  mockConfig,
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
  let project = "rpms/" ++ pkg
  res <- pagureListGitBranches srcfpo project
  return $ either (error' . include project) id res
  where
    include p e = e ++ ": " ++ p

mockConfig :: Branch -> String
mockConfig Master = "fedora-rawhide-x86_64"
mockConfig (Fedora n) = "fedora-" ++ show n ++ "-x86_64"
mockConfig (EPEL n) = "epel-" ++ show n ++ "-x86_64"
