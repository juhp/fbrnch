{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Copr (
  BuildBy(..),
  coprCmd,
  CoprMode(..)
  )
where

import Data.Aeson.Types (Object)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Strict as M
#endif
import Data.Char (isDigit)
import  Data.Either.Combinators (whenLeft)
import Data.Ini.Config
import Data.RPM.NVR (nvrVerRel)
import Data.RPM.VerRel (showVerRel)
import Data.Tuple.Extra (first)
import Distribution.Fedora.Branch (getActiveBranches, getActiveBranched)
import Network.HTTP.Query (lookupKey, lookupKey')
import Safe (headDef)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.Time.Extra (sleep)
import Web.Fedora.Copr (coprChroots, fedoraCopr)
import Web.Fedora.Copr.API (coprGetBuild, coprGetBuildList, coprMonitorProject)

import Branches
import Common
import Common.System
import qualified Common.Text as T
import Package
import RpmBuild (generateSrpmNoDist)
import Types (Archs(..))

data BuildBy = SingleBuild
             | ValidateByRelease
             | ValidateByArch
             | BuildByRelease
  deriving Eq

coprServer :: String
coprServer = "copr.fedorainfracloud.org"

data Chroot = Chroot { chrootBranch :: Branch
                     , chrootArch :: Arch }
  deriving Eq

releaseToBranch :: String -> Branch
releaseToBranch release
  | "fedora-" `isPrefixOf` release =
    case dropPrefix "fedora-" release of
      "rawhide" -> Rawhide
      n | all isDigit n -> Fedora (read n)
        | otherwise -> error' $ "unknown fedora release:" +-+ release
  | "epel-" `isPrefixOf` release =
    let next = "-next" `isSuffixOf` release
    in case dropSuffix "-next" (dropPrefix "epel-" release) of
         n | all isDigit n -> (if next then EPELNext else EPEL) $ read n
           | otherwise -> error' $ "unknown epel release:" +-+ release
  | otherwise = error' $ "unknown release:" +-+ release

branchToRelease :: Branch -> String
branchToRelease Rawhide = "fedora-rawhide"
branchToRelease (Fedora n) = "fedora-" ++ show n
branchToRelease (EPEL n) = "epel-" ++ show n
branchToRelease (EPELNext n) = "epel-" ++ show n ++ "-next"

readChroot :: String -> Chroot
readChroot ch =
  case stripInfixEnd "-" ch of
    Nothing -> error' $ "invalid chroot:" +-+ ch
    Just (b,a) -> Chroot (releaseToBranch b) (readArch a)

showChroot :: Chroot -> String
showChroot (Chroot br ar) = branchToRelease br ++ '-' : showArch ar

data Arch = X86_64 | I686 | AARCH64 | PPC64LE | S390X
  deriving (Eq, Enum)

allCoprArchs :: [Arch]
allCoprArchs = [X86_64 ..]

readArch :: String -> Arch
readArch "x86_64" = X86_64
readArch "i686" = I686
readArch "aarch64" = AARCH64
readArch "ppc64le" = PPC64LE
readArch "s390x" = S390X
readArch a = error' $ "unknown arch:" +-+ a

showArch :: Arch -> String
showArch X86_64 = "x86_64"
showArch I686 = "i686"
showArch AARCH64 = "aarch64"
showArch PPC64LE = "ppc64le"
showArch S390X = "s390x"

data CoprMode = ListChroots | CoprMonitor | CoprBuild | CoprNew

-- FIXME take ExclusiveArch/ExcludeArch into account
-- FIXME -1 for only first unbuilt chroot
-- FIXME check branch
-- FIXME handle fedora-eln
-- FIXME make project optional (if same as pkg??) or configurable ?
-- FIXME repo config with a setup command?
-- FIXME interact with copr dist-git
-- FIXME parallel copr builds
-- FIXME time builds?
coprCmd :: Bool -> CoprMode -> Bool -> Maybe BuildBy -> Maybe Archs -> String
        -> (BranchesReq,[String]) -> IO ()
coprCmd dryrun mode force mbuildBy marchs project (breq, pkgs) = do
  user <- getUsername
  let buildBy = fromMaybe ValidateByRelease mbuildBy
  case mode of
    ListChroots -> coprGetChroots user >>= mapM_ (putStrLn . showChroot)
    CoprMonitor -> coprMonitorPackages user project >>= mapM_ printPkgRes
    CoprNew -> coprNewProject dryrun project marchs breq pkgs
    CoprBuild -> do
      chroots <- coprGetChroots user
      if null pkgs
        then coprBuildPkg user buildBy chroots False
        else
        mapM_ (\(n,p) -> withExistingDirectory p $ coprBuildPkg user buildBy chroots (n>0)) $ zip (reverse [0..length pkgs - 1]) pkgs
  where
    coprGetChroots :: String -> IO [Chroot]
    coprGetChroots user = do
      chroots <- map (readChroot . T.unpack) <$> coprChroots coprServer user project
      when (null chroots) $
        error' $ "No chroots found for" +-+ user ++ "/" ++ project
      branches <-
        case breq of
          Branches brs ->
            if null brs
            then return $ (nub . map chrootBranch) chroots
            else listOfBranches False False breq
          BranchOpt AllFedora -> filter isFedoraBranch <$> getActiveBranches
          BranchOpt AllEPEL -> filter isEPELBranch <$> getActiveBranched
          _ -> listOfBranches False False breq
      let buildroots =
            -- FIXME sort archs appropriately
            case marchs of
              Nothing ->
                [chroot | arch <- sortArchs allCoprArchs, br <- branches, let chroot = Chroot br arch, chroot `elem` chroots]
              -- FIXME warn about missing arch chroots
              Just (Archs archs) ->
                [chroot | arch <- sortArchs (map readArch archs), br <- branches, let chroot = Chroot br arch, chroot `elem` chroots]
              -- FIXME warn about missing arch chroots
              Just (ExcludedArchs exarchs) ->
                [chroot | arch <- sortArchs allCoprArchs, arch `notElem` map readArch exarchs, br <- branches, let chroot = Chroot br arch, chroot `elem` chroots]
      if null buildroots
        then error' "No valid chroots"
        else return buildroots

    coprBuildPkg :: String -> BuildBy -> [Chroot] -> Bool -> IO ()
    coprBuildPkg user buildBy chroots morepkgs = do
      -- FIXME check is pkg.spec
      -- was: localBranchSpecFile pkg (RelBranch Rawhide)
      spec <- findSpecfile
      -- pkg <- takeFileName <$> getCurrentDirectory
      srpm <- if not dryrun
              then generateSrpmNoDist True False Nothing spec
              else return spec -- hack to avoid generating srpm for dryrun
      verrel <- showVerRel . nvrVerRel <$> pkgNameVerRelNodist spec
      actualpkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
      builtChroots <- existingChrootBuilds dryrun user project (Package actualpkg) verrel chroots
      let finalChroots = chroots \\ if force then [] else map taskChroot builtChroots
      case finalChroots of
        [] -> putStrLn $ actualpkg ++ '-' : verrel +-+ "built"
        (final:_) ->
          case buildBy of
            SingleBuild -> coprBuild dryrun user project srpm spec finalChroots
            -- FIXME or default to secondary parallel to previous primary
            ValidateByRelease -> do
              let initialChroots =
                    let primaryArch = chrootArch final
                    in map pure $ filter (isArch primaryArch) finalChroots
                  remainingChroots = finalChroots \\ concat initialChroots
              staggerBuilds srpm spec initialChroots remainingChroots
            ValidateByArch -> do
              let initialChroots =
                    let newestRelease = chrootBranch final
                    in map pure $ filter ((== newestRelease) . chrootBranch) finalChroots
                  remainingChroots = finalChroots \\ concat initialChroots
              staggerBuilds srpm spec initialChroots remainingChroots
            BuildByRelease -> do
              let releaseChroots = groupBy sameRelease finalChroots
              staggerBuilds srpm spec releaseChroots []
      when morepkgs putNewLn
      where
        staggerBuilds :: FilePath -> FilePath -> [[Chroot]] -> [Chroot] -> IO ()
        staggerBuilds srpm spec initialChroots remainingChroots = do
          mapM_ (coprBuild dryrun user project srpm spec) initialChroots
          unless (null remainingChroots) $
            coprBuild dryrun user project srpm spec remainingChroots

    isArch arch release = chrootArch release == arch

    sameRelease r1 r2 = chrootBranch r1 == chrootBranch r2

getUsername :: IO String
getUsername = do
  rc <- getUserConfigDir "copr"
  readIniConfig rc rcParser id
  where
    rcParser :: IniParser String
    rcParser =
      section "copr-cli" $
      fieldOf "username" string

-- changed from Bugzilla.hs
readIniConfig :: FilePath -> IniParser a -> (a -> b) -> IO b
readIniConfig inifile iniparser record = do
  havefile <- doesFileExist inifile
  if not havefile
    then error' $ inifile +-+ "not found: try https://" ++ coprServer ++ "/api"
    else do
    ini <- T.readFile inifile
    let config = parseIniFile ini iniparser
    return $ either error' record config

-- from copr/frontend/coprs_frontend/coprs/logic/builds_logic.py
-- FIXME after adding to copr-api
coprProcessingStates :: [String]
coprProcessingStates =
  ["running", "pending", "starting", "importing", "waiting"]

-- coprEndedStates :: [String]
-- coprEndedStates =
--   ["canceled", "failed", "skipped", "succeeded"]

-- FIXME restrict to requested chroots?
existingChrootBuilds :: Bool -> String -> String -> Package -> String
                     -> [Chroot] -> IO [CoprTask]
existingChrootBuilds dryrun user project actualpkg verrel chroots = do
  monitorPkgs <- coprMonitorPackages user project
  let pkgmonitor = fromMaybe [] $ lookup actualpkg monitorPkgs
  let buildingChroots =
        filterTasks verrel (`elem` coprProcessingStates) pkgmonitor
      builds = filterTasks verrel (`notElem` ["failed","skipped"]) pkgmonitor
  if null buildingChroots
    then return builds
    else do
    mapM_ printCoprTask buildingChroots
    putNewLn
    unless dryrun $
      forM_ buildingChroots $ \building ->
      when (taskChroot building `elem` chroots) $
      -- FIXME check failure
      void $ coprWatchBuild Nothing $ Left building
    return builds

filterTasks :: String -> (String -> Bool) -> [CoprTask] -> [CoprTask]
filterTasks verrel statustest =
  filter (\CoprTask{..} -> taskVerRel == verrel && statustest taskStatus)

coprBuild :: Bool -> String -> String -> FilePath -> FilePath -> [Chroot] -> IO ()
coprBuild _ _ _ _ _ [] = error' "No chroots chosen"
coprBuild dryrun user project srpm spec chroots = do
  let chrootargs = mconcat [["-r", showChroot bldrt] | bldrt <- chroots]
      buildargs = ["build", "--nowait"] ++ chrootargs ++ [project, srpm]
  putNewLn
  putStrLn $ unwords $ map showChroot chroots
  unless dryrun $ do
    output <- cmd "copr" buildargs
    putStrLn output
    let bid = read $ last $ words $ last $ lines output
    -- FIXME get the actual build time
    ok <- timeIO $ coprWatchBuild Nothing $ Right bid
    unless ok $ do
      putStrLn $ "Failed: copr" +-+ unwords buildargs
      -- FIXME determine which chroot(s) failed
      -- eg 06482247
      let zbid =
            let s = show bid
            in (if length s < 8 then ('0' :) else id) s
      actualpkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
      -- FIXME which chroot?
      -- FIXME print buildlog size
      error' $ "https://download.copr.fedorainfracloud.org/results" +/+ user +/+ project +/+ showChroot (headDef (error' " nochroot!") chroots) +/+ zbid ++ "-" ++ actualpkg +/+ "builder-live.log.gz"

-- FIXME idea: Maybe Seconds to increment FIXME
-- sleep should have all chroots in pending CoprTask build
coprWatchBuild :: Maybe String -> Either CoprTask Int -> IO Bool
coprWatchBuild mstate ebuild = do
  let bid = either taskBuild id ebuild
  res <- coprGetBuild fedoraCopr bid
  case lookupKey "state" res of
    Just state ->
      if mstate == Just state
      then sleep 20 >> coprWatchBuild mstate (Right bid)
      else do
        whenLeft ebuild $ \t -> putStrLn $ "#" +-+ showChroot (taskChroot t)
        logMsg $ "Build" +-+ show bid +-+ state +-+ either taskVerRel (const "") ebuild
        case state of
          "succeeded" -> return True
          "skipped" -> return True
          "canceled" -> return False
          "failed" -> return False
          _ -> sleep 1 >> coprWatchBuild (Just state) (Right bid)
    Nothing -> do
      let err = lookupKey' "error" res
      logMsg $ "Error:" +-+ err
      return False

archPriorities :: [(Arch,Int)]
archPriorities = [(X86_64,1), (AARCH64,2)]

sortArchs :: [Arch] -> [Arch]
sortArchs = sortBy priority
  where
    priority a1 a2 =
      let mp1 = lookup a1 archPriorities
          mp2 = lookup a2 archPriorities
      in
        case (mp1,mp2) of
          (Just p1,Just p2) ->
            compare p1 p2
          (Just _, Nothing) -> LT
          (Nothing, Just _) -> GT
          (Nothing,Nothing) -> EQ

type CoprPackage = (Package,[CoprTask])

data CoprTask = CoprTask {taskChroot :: Chroot,
                          taskBuild :: Int,
                          taskStatus :: String,
                          taskVerRel :: String
                         }

-- FIXME wait for None package to appear
coprMonitorPackages :: String -> String -> IO [CoprPackage]
coprMonitorPackages user proj = do
  builds <- coprGetBuildList fedoraCopr user proj []
  mapM_ coprWaitPackage (lookupKey' "items" builds :: [Object])
  mapMaybe pkgResults . lookupKey' "packages" <$>
    coprMonitorProject fedoraCopr user proj []
  where
    pkgResults :: Object -> Maybe CoprPackage
    pkgResults obj = do
      name <- lookupKey "name" obj
      chroots <- map (first toText) . M.toList <$> lookupKey "chroots" obj
      return (Package name, mapMaybe chrootResult chroots)
#if !MIN_VERSION_aeson(2,0,0)
      where toText = id
#endif

    chrootResult :: (T.Text,Object) -> Maybe CoprTask
    chrootResult (chroot, obj) = do
      state <- lookupKey "state" obj
      version <- lookupKey "pkg_version" obj
      build <- lookupKey "build_id" obj
      return $ CoprTask (readChroot (T.unpack chroot)) build state version

printPkgRes :: (Package, [CoprTask]) -> IO ()
printPkgRes (pkg,chroots) = do
  putStrLn $ "# " <> unPackage pkg
  mapM_ printCoprTask chroots
  putNewLn

printCoprTask :: CoprTask -> IO ()
printCoprTask (CoprTask chr build status version) =
  putStrLn $ showChroot chr +-+ show build ++ ":" +-+ status +-+ version

coprWaitPackage :: Object -> IO ()
coprWaitPackage build = do
  case buildResult of
    Nothing -> error' "unknown source_package"
    Just (_, "failed", _) -> return ()
    Just (_, _, Just _pkg) -> return ()
    Just (bid, _, Nothing) -> do
      sleep 5
      print bid
      coprGetBuild fedoraCopr bid >>= coprWaitPackage
  where
    buildResult = do
      bid <- lookupKey "id" build
      source <- lookupKey "source_package" build
      state <- lookupKey "state" build
      let mname = lookupKey "name" source
      return (bid :: Int, state :: String, mname :: Maybe String)

coprNewProject :: Bool -> String -> Maybe Archs -> BranchesReq -> [String]
               -> IO ()
coprNewProject dryrun project marchs breq pkgs = do
  branches <- listOfBranches False False breq
  let archs =
        case marchs of
          Nothing -> [X86_64]
          Just (Archs as) -> map readArch as
          Just (ExcludedArchs eas) -> allCoprArchs \\ map readArch eas
  let chroots = [ Chroot b a | b <- branches, a <- archs]
  (if dryrun then cmdN else cmd_) "copr" $ "create" : concatMap (\ch -> ["--chroot", showChroot ch]) chroots ++ [project]
  unless (null pkgs) $
    coprCmd False CoprBuild False Nothing Nothing project (breq, pkgs)
