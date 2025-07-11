{-# LANGUAGE CPP #-}

module Branches (
  activeBranches,
  fedoraBranches,
  fedoraBranchesNoRawhide,
  isFedoraBranch,
  isEPELBranch,
  localBranches,
  pagurePkgBranches,
  mockRoot,
  Branch(..),
  showBranch,
  AnyBranch(..),
  anyBranch,
  isRelBranch,
  onlyRelBranch,
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
  BranchesReq(..),
  gitLines,
  renameMasterToRawhide
) where

import Data.Time.Calendar (diffDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Either (partitionEithers)
import Distribution.Fedora.Branch (Branch(..), branchRelease,
                                   eitherBranch, getActiveBranched,
                                   getActiveBranches, getLatestFedoraBranch,
                                   readActiveBranch, eitherActiveBranch,
                                   readBranch, showBranch)
import Distribution.Fedora.Release (Release(releaseEOL))
import SimpleCmd.Git
import SimplePrompt (promptEnter, promptInitial)
import qualified System.Info (arch)

import Common
import Common.System
import Pagure

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
  show (RelBranch br) = showBranch br
  show (OtherBranch obr) = obr

activeBranches :: [Branch] -> [String] -> [Branch]
activeBranches active =
  -- newest branch first
  reverseSort . mapMaybe (readActiveBranch active)

fedoraBranches :: IO [String] -> IO [Branch]
fedoraBranches mthd = do
  active <- getActiveBranches
  activeBranches active <$> mthd

fedoraBranchesNoRawhide :: IO [String] -> IO [Branch]
fedoraBranchesNoRawhide mthd = do
  active <- getActiveBranched
  activeBranches active <$> mthd

isFedoraBranch :: Branch -> Bool
isFedoraBranch (Fedora _) = True
isFedoraBranch Rawhide = True
isFedoraBranch _ = False

isEPELBranch :: Branch -> Bool
isEPELBranch (EPELNext _) = True
isEPELBranch (EPEL _) = True
isEPELBranch (EPELMinor _ _) = True
isEPELBranch _ = False

localBranches :: Bool -> IO [String]
localBranches local =
  if local
  then do
    locals <- gitLines "branch" ["--list", "--format=%(refname:lstrip=-1)"]
    return $ locals \\ ["HEAD", "master"]
  else do
    origins <-
      filter ("origin/" `isPrefixOf`) <$>
      gitLines "branch" ["--remote", "--list", "--format=%(refname:lstrip=-2)"]
    return $ map (removePrefix "origin/") origins \\ ["HEAD", "master"]

-- FIXME use Package?
pagurePkgBranches :: String -> IO [String]
pagurePkgBranches pkg = do
  let project = "rpms/" ++ pkg
  res <- pagureListGitBranches srcfpo project
  return $ either (error' . include project) id res
  where
    include p e = e ++ ":" +-+ p

mockRoot :: Branch -> Maybe String -> String
mockRoot br march =
  let arch = fromMaybe System.Info.arch march
  in
    case br of
      Rawhide -> "fedora-rawhide-" ++ arch
      Fedora n -> "fedora-" ++ show n ++ "-" ++ arch
      EPEL n -> "centos-stream+epel-" ++ show n ++ "-" ++ arch
      EPELNext n -> "centos-stream+epel-next-" ++ show n ++ "-" ++ arch
      EPELMinor n _ ->  "rhel+epel-" ++ show n ++ "-" ++ arch

------

data BranchOpts = AllBranches | AllFedora | AllEPEL | ExcludeBranches [Branch]
  deriving Eq

onlyRelBranch :: AnyBranch -> Branch
onlyRelBranch (RelBranch br) = br
onlyRelBranch (OtherBranch br) = error' $ "Non-release branch not allowed:" +-+ br

systemBranch :: IO Branch
systemBranch = do
  mplatform <- listToMaybe <$> grep "PLATFORM_ID=" "/etc/os-release"
  case mplatform of
    Just p -> do
      -- PLATFORM_ID="platform:f42"
      let platform = takeWhileEnd (/= ':') $ dropSuffix "\"" p
      if platform == "eln"
        then return Rawhide
        else
        case readBranch platform of
          Just br -> do
            branched <- getLatestFedoraBranch
            return $
              if br > branched
              then Rawhide
              else br
          Nothing -> error' $ "unknown platform" +-+ platform
    Nothing -> do
      mcpe <- listToMaybe <$> grep "CPE_NAME=" "/etc/os-release"
      case mcpe of
        Just c -> do
          -- CPE_NAME="cpe:/o:redhat:enterprise_linux:9::baseos"
          -- CPE_NAME="cpe:/o:fedoraproject:fedora:43"
          let cpe = takeEnd 2 $ splitOn ":" $ dropPrefix "CPE_NAME=\"cpe:/o:" $ dropSuffix "\"" $ fst $ breakOn "::" c
          case cpe of
            [os,ver] ->
              if ver == "eln"
              then return Rawhide
              else
                case readBranch $ osPrefix os ++ ver of
                  Just br -> do
                    branched <- getLatestFedoraBranch
                    return $
                      if br > branched
                      then Rawhide
                      else br
                  Nothing -> error' "could not map CPE_NAME to branch"
            _ -> error' $ "could not parse:" +-+ c
        Nothing -> error' "CPE_NAME not found in /etc/os-release"
    where
      osPrefix "fedora" = "f"
      osPrefix "centos" = "epel"
      osPrefix "enterprise_linux" = "epel"
      osPrefix s = s

listOfBranches :: Bool -> Bool -> BranchesReq -> IO [Branch]
listOfBranches distgit _active (BranchOpt AllBranches) =
  if distgit
  then fedoraBranches (localBranches False)
  else getActiveBranches
listOfBranches distgit _active (BranchOpt AllFedora) =
  filter isFedoraBranch <$>
  if distgit
  then fedoraBranches (localBranches False)
  else getActiveBranches
listOfBranches distgit _active (BranchOpt AllEPEL) =
  filter isEPELBranch <$>
  if distgit
  then fedoraBranches (localBranches False)
  else getActiveBranches
listOfBranches distgit _ (BranchOpt (ExcludeBranches brs)) = do
  branches <- if distgit
              then fedoraBranches (localBranches False)
              else getActiveBranches
  return $ branches \\ brs
listOfBranches distgit active (Branches brs) =
  if null brs
  then
    pure <$> if distgit
             then getReleaseBranch
             else systemBranch
  else do
    activeBrs <- getActiveBranches
    forM_ brs $ \ br ->
          if active
            then do
            unless (br `elem` activeBrs) $
              error' $ showBranch br +-+ "is not an active branch"
            rel <- branchRelease br
            let meol = releaseEOL rel
            whenJust meol $ \eol -> do
              let date = read eol
              today <- utctDay <$> getCurrentTime
              let diff = diffDays date today
              when (diff <= 7) $
                warning $ "EOL for" +-+ showBranch br +-+ "is" +-+ show diff +-+ "days away!"
            else
            case br of
              Fedora _ -> do
                let latest = maximum (delete Rawhide activeBrs)
                when (br > latest) $
                  error' $ showBranch br +-+ "is newer than latest branch"
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
    promptEnter $ dir ++ ":" +-+ show br +-+ "is not a branch, please fix"
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
    promptEnter $ dir +-+ show br +-+ "is not a valid branch: press Enter to rename to rawhide"
    renameMasterToRawhide
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
branchVersion (EPELNext n) = show n
branchVersion (EPELMinor n _) = show n

getRequestedBranches :: [String] -> BranchesReq -> IO [Branch]
getRequestedBranches existing breq = do
  activenew <- filter (\b -> showBranch b `notElem` existing) <$> getActiveBranched
  case breq of
    Branches brs -> if null brs
                    then branchingPrompt activenew
                    else return $ [b | b <- brs, b `elem` activenew]
    BranchOpt request -> do
      let requested = case request of
                        AllBranches -> activenew
                        AllFedora -> filter isFedoraBranch activenew
                        AllEPEL -> filter isEPELBranch activenew
                        ExcludeBranches xbrs -> activenew \\ xbrs
      confirmBranches activenew requested
  where
    branchingPrompt :: [Branch] -> IO [Branch]
    branchingPrompt active = do
      inp <- promptInitial "Enter required branches" $
             unwords $ map showBranch $ take 2 active
      let abrs = map anyBranch $ words inp
        in if all isRelBranch abrs
           then return $ map onlyRelBranch abrs
           else branchingPrompt active

    confirmBranches :: [Branch] -> [Branch] -> IO [Branch]
    confirmBranches activenew requested = do
      inp <- promptInitial "Confirm branches to request" $ unwords (map showBranch requested)
      let (errs,oks) = partitionEithers $
                       map (eitherActiveBranch activenew) $ words inp
      if null errs
        then return oks
        else do
        putStrLn $ "unknown branches:" +-+ unwords errs
        confirmBranches activenew requested

data BranchesReq =
  BranchOpt BranchOpts | Branches [Branch]
  deriving Eq

gitLines :: String -> [String] -> IO [String]
gitLines c args = lines <$> git c args

renameMasterToRawhide :: IO ()
renameMasterToRawhide = do
  locals <- gitLines "branch" ["--format=%(refname:short)"]
  -- FIXME dangling warning in current output:
    -- From ssh://pkgs.fedoraproject.org/rpms/hedgewars
    --  - [deleted]         (none)     -> origin/master
    --    (refs/remotes/origin/HEAD has become dangling)
    -- Branch 'rawhide' set up to track remote branch 'rawhide' from 'origin'.
  -- compare commands with github rename
  unless ("rawhide" `elem` locals) $ do
    git_ "fetch" ["--prune"]
    git_ "branch" ["--move", "master", "rawhide"]
    git_ "remote" ["set-head", "origin", "rawhide"]
    git_ "branch" ["--set-upstream-to", "origin/rawhide", "rawhide"]
    git_ "pull" []
