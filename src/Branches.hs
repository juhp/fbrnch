module Branches (
  activeBranches,
  fedoraBranches,
  fedoraBranchesNoRawhide,
  isFedoraBranch,
  isEPELBranch,
  localBranches,
  pagurePkgBranches,
  mockConfig,
  module Distribution.Fedora.Branch,
  AnyBranch(..),
  anyBranch,
  isRelBranch,
  onlyRelBranch,
  partitionBranches,
  BranchOpts(..),
  listOfBranches,
  gitCurrentBranch,
  systemBranch,
  getReleaseBranch,
  branchVersion,
  anyBranchToRelease,
  getRequestedBranches,
  BranchesReq(..)
) where

import Common

import Data.Either
import Data.Tuple
import Distribution.Fedora.Branch
import SimpleCmd
import SimpleCmd.Git

import Pagure
import Prompt

data AnyBranch = RelBranch Branch | OtherBranch String
  deriving Eq

anyBranch :: String -> AnyBranch
anyBranch = either OtherBranch RelBranch . eitherBranch

-- allRelBranches :: [AnyBranch] -> Bool
-- allRelBranches = all isRelBranch

isRelBranch :: AnyBranch -> Bool
isRelBranch (RelBranch _) = True
isRelBranch _ = False

instance Show AnyBranch where
  show (RelBranch br) = show br
  show (OtherBranch obr) = obr

partitionBranches :: [String] -> ([Branch],[String])
partitionBranches args =
  swap . partitionEithers $ map eitherBranch args

activeBranches :: [Branch] -> [String] -> [Branch]
activeBranches active =
  -- newest branch first
  reverse . sort . mapMaybe (readActiveBranch active)

fedoraBranches :: IO [String] -> IO [Branch]
fedoraBranches mthd = do
  active <- getFedoraBranches
  activeBranches active <$> mthd

fedoraBranchesNoRawhide :: IO [String] -> IO [Branch]
fedoraBranchesNoRawhide mthd = do
  active <- getFedoraBranched
  activeBranches active <$> mthd

isFedoraBranch :: Branch -> Bool
isFedoraBranch (Fedora _) = True
isFedoraBranch Rawhide = True
isFedoraBranch _ = False

isEPELBranch :: Branch -> Bool
isEPELBranch (EPEL _) = True
isEPELBranch _ = False

-- FIXME misnamed
localBranches :: IO [String]
localBranches = do
  origins <- filter ("origin/" `isPrefixOf`) <$> cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-2)"]
  return $ map (removePrefix "origin/") origins \\ ["HEAD", "master"]

pagurePkgBranches :: String -> IO [String]
pagurePkgBranches pkg = do
  let project = "rpms/" ++ pkg
  res <- pagureListGitBranches srcfpo project
  return $ either (error' . include project) id res
  where
    include p e = e ++ ": " ++ p

mockConfig :: Branch -> String
mockConfig Rawhide = "fedora-rawhide-x86_64"
mockConfig (Fedora n) = "fedora-" ++ show n ++ "-x86_64"
mockConfig (EPEL n) = "epel-" ++ show n ++ "-x86_64"

------

data BranchOpts = AllBranches | AllFedora | AllEPEL | ExcludeBranches [Branch]
  deriving Eq

onlyRelBranch :: AnyBranch -> Branch
onlyRelBranch (RelBranch br) = br
onlyRelBranch (OtherBranch br) = error' $ "Non-release branch not allowed: " ++ br

systemBranch :: IO Branch
systemBranch =
  readBranch' . init . removePrefix "PLATFORM_ID=\"platform:" <$> cmd "grep" ["PLATFORM_ID=", "/etc/os-release"]

listOfBranches :: Bool -> Bool -> BranchesReq -> IO [Branch]
listOfBranches distgit _active (BranchOpt AllBranches) =
  if distgit
  then fedoraBranches localBranches
  else error' "--all-branches only allowed for dist-git packages"
listOfBranches distgit _active (BranchOpt AllFedora) =
  if distgit
  then filter isFedoraBranch <$> fedoraBranches localBranches
  else error' "--all-fedora only allowed for dist-git packages"
listOfBranches distgit _active (BranchOpt AllEPEL) =
  if distgit
  then filter isEPELBranch <$> fedoraBranches localBranches
  else error' "--all-epel only allowed for dist-git packages"
listOfBranches distgit _ (BranchOpt (ExcludeBranches brs)) = do
  branches <- if distgit
              then fedoraBranches localBranches
              else getFedoraBranches
  return $ branches \\ brs
listOfBranches distgit active (Branches brs) =
  if null brs
  then
    pure <$> if distgit
             then getReleaseBranch
             else systemBranch
  else do
    activeBrs <- getFedoraBranches
    forM_ brs $ \ br ->
          if active
            then when (br `notElem` activeBrs) $
                 error' $ show br ++ " is not an active branch"
            else
            case br of
              Fedora _ -> do
                let latest = maximum (delete Rawhide activeBrs)
                when (br > latest) $
                  error' $ show br ++ " is newer than latest branch"
              -- FIXME also check for too new EPEL
              _ -> return ()
    return brs

getReleaseBranch :: IO Branch
getReleaseBranch =
  gitCurrentBranch >>= anyBranchToRelease

gitCurrentBranch :: IO AnyBranch
gitCurrentBranch =
  anyBranch <$> git "rev-parse" ["--abbrev-ref", "HEAD"]

anyBranchToRelease :: AnyBranch -> IO Branch
anyBranchToRelease (RelBranch rbr) = return rbr
anyBranchToRelease (OtherBranch _) = systemBranch

-- move to fedora-dists
branchVersion :: Branch -> String
branchVersion Rawhide = "rawhide"
branchVersion (Fedora n) = show n
branchVersion (EPEL n) = show n

getRequestedBranches :: BranchesReq -> IO [Branch]
getRequestedBranches breq = do
  active <- getFedoraBranched
  case breq of
    Branches brs -> if null brs
                    then branchingPrompt
                    else return brs
    BranchOpt request -> do
      let requested = case request of
                        AllBranches -> active
                        AllFedora -> filter isFedoraBranch active
                        AllEPEL -> filter isEPELBranch active
                        ExcludeBranches xbrs -> active \\ xbrs
      inp <- prompt $ "Confirm branches request [" ++ unwords (map show requested) ++ "]"
      return $ if null inp
               then requested
               else map (readActiveBranch' active) $ words inp
  where
    branchingPrompt :: IO [Branch]
    branchingPrompt = do
      -- FIXME default may be wrong now
      inp <- prompt "Enter required branches [default: latest 2]"
      if null inp
        then return []
        else
        let abrs = map anyBranch $ words inp
        in if all isRelBranch abrs
           then return $ map onlyRelBranch abrs
           else branchingPrompt

data BranchesReq =
  BranchOpt BranchOpts | Branches [Branch]
  deriving Eq
