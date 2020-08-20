module Branches (
  fedoraBranches,
  fedoraBranchesNoMaster,
  localBranches,
  pagurePkgBranches,
  mockConfig,
  module Distribution.Fedora.Branch,
  AnyBranch(..),
  anyBranch,
  onlyRelBranch,
  Branches(..),
  someBranches,
  maybeBranches,
  anyBranches,
  listOfBranches,
  gitCurrentBranch,
  systemBranch,
  getReleaseBranch
) where

import Common

import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git

import Pagure

data AnyBranch = RelBranch Branch | OtherBranch String
  deriving Eq

anyBranch :: String -> AnyBranch
anyBranch = either OtherBranch RelBranch . eitherBranch

-- isRelBranch :: AnyBranch -> Bool
-- isRelBranch (RelBranch _) = True
-- isRelBranch _ = False

instance Show AnyBranch where
  show (RelBranch br) = show br
  show (OtherBranch obr) = obr

activeBranches :: [Branch] -> [String] -> [Branch]
activeBranches active =
  -- newest branch first
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

data Branches = AllBranches | BranchList [Branch] | ExcludeBranches [Branch]
              | AnotherBranch String
  deriving Eq

maybeBranches :: Maybe Branch -> Branches
maybeBranches = BranchList . maybeToList

someBranches :: Branches -> Bool
someBranches AllBranches = True
someBranches (BranchList brs) = length brs > 1
someBranches (ExcludeBranches _) = True
someBranches (AnotherBranch _) = False

anyBranches :: AnyBranch -> Branches
anyBranches (RelBranch br) = BranchList [br]
anyBranches (OtherBranch obr) = AnotherBranch obr

onlyRelBranch :: AnyBranch -> Branch
onlyRelBranch (RelBranch br) = br
onlyRelBranch (OtherBranch br) = error' $ "Non-release branch not allowed: " ++ br

systemBranch :: IO Branch
systemBranch =
  readBranch' . init . removePrefix "PLATFORM_ID=\"platform:" <$> cmd "grep" ["PLATFORM_ID=", "/etc/os-release"]

listOfBranches :: Bool -> Bool -> Branches -> IO [AnyBranch]
listOfBranches distgit _active AllBranches =
  -- FIXME for status had: RemoteBranches -> fedoraBranches $ pagurePkgBranches (unPackage pkg)
  if distgit
  then map RelBranch <$> fedoraBranches localBranches
  else error' "--all-branches only allowed for dist-git packages"
listOfBranches distgit active (BranchList brs) =
  if null brs
  then
    pure <$> if distgit
             then gitCurrentBranch
             else RelBranch <$> systemBranch
  else do
    when active $ do
      activeBrs <- getFedoraBranches
      forM_ brs $ \ br ->
        unless (br `elem` activeBrs) $
        error' $ show br ++ " is not an active branch"
    return $ map RelBranch brs
listOfBranches distgit _ (ExcludeBranches brs) = do
  branches <- if distgit
              then fedoraBranches localBranches
              else getFedoraBranches
  return $ map RelBranch (branches \\ brs)
listOfBranches _distgit _ (AnotherBranch obr) =
  return [OtherBranch obr]

getReleaseBranch :: IO Branch
getReleaseBranch = do
  mcurrent <- gitCurrentBranch
  case mcurrent of
    RelBranch br -> return br
    _ -> systemBranch

gitCurrentBranch :: IO AnyBranch
gitCurrentBranch =
  anyBranch <$> git "rev-parse" ["--abbrev-ref", "HEAD"]
