module Branches (
  activeBranches,
  fedoraBranches,
  fedoraBranchesNoRawhide,
  isFedoraBranch,
  isEPELBranch,
  localBranches,
  pagurePkgBranches,
  mockRoot,
  module Distribution.Fedora.Branch,
  AnyBranch(..),
  anyBranch,
  isRelBranch,
  onlyRelBranch,
  partitionBranches,
  BranchOpts(..),
  listOfBranches,
  listOfAnyBranches,
  gitCurrentBranch,
  gitCurrentBranch',
  checkOnBranch,
  systemBranch,
  getReleaseBranch,
  getReleaseBranchWarn,
  branchVersion,
  anyBranchToRelease,
  getRequestedBranches,
  BranchesReq(..)
) where

import Common
import Common.System

import Data.Either
import Data.Tuple
import Distribution.Fedora.Branch
import SimpleCmd.Git
import System.Info (arch)

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

localBranches :: Bool -> IO [String]
localBranches local =
  if local
  then do
    locals <- cmdLines "git" ["branch", "--list", "--format=%(refname:lstrip=-1)"]
    return $ locals \\ ["HEAD", "master"]
  else do
    origins <- filter ("origin/" `isPrefixOf`) <$> cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-2)"]
    return $ map (removePrefix "origin/") origins \\ ["HEAD", "master"]

pagurePkgBranches :: String -> IO [String]
pagurePkgBranches pkg = do
  let project = "rpms/" ++ pkg
  res <- pagureListGitBranches srcfpo project
  return $ either (error' . include project) id res
  where
    include p e = e ++ ": " ++ p

mockRoot :: Branch -> String
mockRoot br = do
  case br of
    Rawhide -> "fedora-rawhide-" ++ arch
    Fedora n -> "fedora-" ++ show n ++ "-" ++ arch
    EPEL n -> "epel-" ++ show n ++ "-" ++ arch

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
  then fedoraBranches (localBranches False)
  else error' "--all-branches only allowed for dist-git packages"
listOfBranches distgit _active (BranchOpt AllFedora) =
  if distgit
  then filter isFedoraBranch <$> fedoraBranches (localBranches False)
  else error' "--all-fedora only allowed for dist-git packages"
listOfBranches distgit _active (BranchOpt AllEPEL) =
  if distgit
  then filter isEPELBranch <$> fedoraBranches (localBranches False)
  else error' "--all-epel only allowed for dist-git packages"
listOfBranches distgit _ (BranchOpt (ExcludeBranches brs)) = do
  branches <- if distgit
              then fedoraBranches (localBranches False)
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

listOfAnyBranches :: Bool -> Bool -> BranchesReq -> IO [AnyBranch]
listOfAnyBranches distgit active breq =
  if breq == Branches [] && distgit
  then pure <$> gitCurrentBranch
  else fmap RelBranch <$> listOfBranches distgit active breq

getReleaseBranch :: IO Branch
getReleaseBranch =
  gitCurrentBranch >>= anyBranchToRelease

getReleaseBranchWarn :: IO Branch
getReleaseBranchWarn =
  gitCurrentBranchWarn >>= anyBranchToRelease

gitCurrentBranch :: IO AnyBranch
gitCurrentBranch = do
  br <- gitCurrentBranch'
  if br == OtherBranch "HEAD"
    then do
    dir <- getDirectoryName
    prompt_ $ dir ++ ": " ++ show br ++ " is not a branch, please fix"
    gitCurrentBranch
    else return br

gitCurrentBranch' :: IO AnyBranch
gitCurrentBranch' = do
  anyBranch <$> git "rev-parse" ["--abbrev-ref", "HEAD"]

gitCurrentBranchWarn :: IO AnyBranch
gitCurrentBranchWarn = do
  br <- gitCurrentBranch
  if br == OtherBranch "master"
    then do
    dir <- getDirectoryName
    prompt_ $ dir ++ ": " ++ show br ++ " is not a valid branch, please use 'rename-rawhide'"
    gitCurrentBranchWarn
    else return br


checkOnBranch :: IO ()
checkOnBranch = void gitCurrentBranch

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
                    then branchingPrompt active
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
    branchingPrompt :: [Branch] -> IO [Branch]
    branchingPrompt active = do
      inp <- prompt "Enter required branches [default: latest 2], or no/none"
      if null inp
        then return $ take 2 active
        else
        if lower (trim inp) `elem` ["no", "none"]
        then return []
        else
          let abrs = map anyBranch $ words inp
          in if all isRelBranch abrs
             then return $ map onlyRelBranch abrs
             else branchingPrompt active

data BranchesReq =
  BranchOpt BranchOpts | Branches [Branch]
  deriving Eq
