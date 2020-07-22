module Branches (
  fedoraBranches,
  fedoraBranchesNoMaster,
  localBranches,
  pagurePkgBranches,
  mockConfig,
  module Distribution.Fedora.Branch,
  Branches(..),
  maybeBranches,
  listOfBranches,
  gitCurrentBranch,
  systemBranch
) where

import Common

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git

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
localBranches = do
  origins <- filter ("origin/" `isPrefixOf`) <$> cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-2)"]
  return $ map (removePrefix "origin/") origins

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

------

data Branches = AllBranches | BranchList [Branch]
  deriving Eq

maybeBranches :: Maybe Branch -> Branches
maybeBranches = BranchList . maybeToList

systemBranch :: IO Branch
systemBranch =
  readBranch' . init . removePrefix "PLATFORM_ID=\"platform:" <$> cmd "grep" ["PLATFORM_ID=", "/etc/os-release"]

listOfBranches :: Bool -> Branches -> IO [Branch]
listOfBranches distgit AllBranches =
  -- FIXME for status had: RemoteBranches -> fedoraBranches $ pagurePkgBranches (unPackage pkg)
  if distgit
  then fedoraBranches localBranches
  else error' "--all-branches only allowed for dist-git packages"
listOfBranches distgit (BranchList brs) =
  if null brs
  then pure <$> if distgit
                then gitCurrentBranch
                else systemBranch
  else return brs

gitCurrentBranch :: IO Branch
gitCurrentBranch = do
  active <- getFedoraBranches
  readActiveBranch' active <$> git "rev-parse" ["--abbrev-ref", "HEAD"]
