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
import Data.Ini.Config
import Data.RPM.NVR (nvrVerRel)
import Data.RPM.VerRel (showVerRel)
import Data.Tuple.Extra (first)
import Network.HTTP.Query (lookupKey, lookupKey')
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.Time.Extra (sleep)
import Web.Fedora.Copr (coprChroots, fedoraCopr)
import Web.Fedora.Copr.API (coprGetBuild, coprMonitorProject)

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

data CoprMode = ListChroots | CoprMonitor | CoprBuild

-- FIXME check branch
-- FIXME handle fedora-eln
-- FIXME make project optional (if same as pkg??) or configurable ?
-- FIXME repo config with a setup command?
-- FIXME interact with copr dist-git
-- FIXME parallel copr builds
-- FIXME skip existing builds **
-- FIXME time builds?
coprCmd :: Bool -> CoprMode -> BuildBy -> Maybe Archs -> String
        -> (BranchesReq,[String]) -> IO ()
coprCmd dryrun mode buildBy marchs project (breq, pkgs) = do
  user <- getUsername
  case mode of
    ListChroots -> coprGetChroots user >>= mapM_ (putStrLn . showChroot)
    CoprMonitor -> coprMonitor user project
    CoprBuild -> do
      chroots <- coprGetChroots user
      if null pkgs
        then do
        pkg <- do
          dirpkg <- getPackageName "."
          exists <- doesFileExist $ packageSpec dirpkg
          if exists
            then return dirpkg
            else Package . takeBaseName <$> findSpecfile
        coprBuildPkg user chroots False pkg
        else
        mapM_ (\(n,p) -> withExistingDirectory p $ coprBuildPkg user chroots (n>0) (Package p)) $ zip (reverse [0,length pkgs - 1]) pkgs
  where
    coprGetChroots :: String -> IO [Chroot]
    coprGetChroots username = do
      chroots <- map (readChroot . T.unpack) <$> coprChroots coprServer username project
      when (null chroots) $
        error' $ "No chroots found for" +-+ username ++ "/" ++ project
      branches <-
        case breq of
          Branches brs ->
            if null brs
            then return $ (nub . map chrootBranch) chroots
            else listOfBranches False False breq
          BranchOpt AllFedora -> filter isFedoraBranch <$> getFedoraBranches
          BranchOpt AllEPEL -> filter isEPELBranch <$> getFedoraBranched
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

    coprBuildPkg :: String -> [Chroot] -> Bool -> Package -> IO ()
    coprBuildPkg user buildroots morepkgs pkg = do
      -- FIXME check is pkg.spec
      -- was: localBranchSpecFile pkg (RelBranch Rawhide)
      spec <- findSpecfile
      -- pkg <- takeFileName <$> getCurrentDirectory
      srpm <- if not dryrun
              then generateSrpmNoDist True False Nothing spec
              else return spec -- hack to avoid generating srpm for dryrun
      verrel <- showVerRel . nvrVerRel <$> pkgNameVerRelNodist spec
      monitorPkgs <- coprMonitorPackages user project
      let monitor = lookup pkg monitorPkgs
          builtChroots =
            case monitor of
              Nothing -> []
              Just tasks ->
                filter (\CoprTask{..} -> taskVerRel == verrel && taskStatus /= "failed") tasks
          finalChroots = buildroots \\ map taskChroot builtChroots
      case buildBy of
        SingleBuild -> coprBuild dryrun project srpm spec finalChroots
        -- FIXME or default to secondary parallel to previous primary
        ValidateByRelease -> do
          let initialChroots =
                let primaryArch = chrootArch $ head finalChroots
                in map pure $ filter (isArch primaryArch) finalChroots
              remainingChroots = finalChroots \\ concat initialChroots
          staggerBuilds srpm spec initialChroots remainingChroots
        ValidateByArch -> do
          let initialChroots =
                let newestRelease = chrootBranch $ head finalChroots
                in map pure $ filter ((== newestRelease) . chrootBranch) finalChroots
              remainingChroots = finalChroots \\ concat initialChroots
          staggerBuilds srpm spec initialChroots remainingChroots
        BuildByRelease -> do
          let releaseChroots = groupBy sameRelease finalChroots
          staggerBuilds srpm spec releaseChroots []
      when morepkgs putNewLn

    staggerBuilds srpm spec initialChroots remainingChroots = do
      mapM_ (coprBuild dryrun project srpm spec) initialChroots
      unless (null remainingChroots) $
        coprBuild dryrun project srpm spec remainingChroots

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

coprBuild :: Bool -> String -> FilePath -> FilePath -> [Chroot] -> IO ()
coprBuild _ _ _ _ [] = error' "No chroots chosen"
coprBuild dryrun project srpm spec buildroots = do
  let chrootargs = mconcat [["-r", showChroot bldrt] | bldrt <- buildroots]
      buildargs = ["build", "--nowait"] ++ chrootargs ++ [project, srpm]
  putNewLn
  putStrLn $ unwords $ map showChroot buildroots
  unless dryrun $ do
    output <- cmd "copr" buildargs
    putStrLn output
    let bid = read $ last $ words $ last $ lines output
    ok <- timeIO $ coprWatchBuild bid Nothing
    unless ok $ do
      putStrLn $ "Failed: copr" +-+ unwords buildargs
      -- FIXME determine which chroot(s) failed
      username <- getUsername
      -- eg 06482247
      let zbid =
            let s = show bid
            in (if length s < 8 then ('0' :) else id) s
      actualpkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
      -- FIXME which chroot?
      -- FIXME print buildlog size
      error' $ "https://download.copr.fedorainfracloud.org/results" +/+ username +/+ project +/+ showChroot (head buildroots) +/+ zbid ++ "-" ++ actualpkg +/+ "builder-live.log.gz"

-- FIXME idea: Maybe Seconds to increment sleep
coprWatchBuild :: Int -> Maybe String -> IO Bool
coprWatchBuild bid mstate = do
  res <- coprGetBuild fedoraCopr bid
  case lookupKey "state" res of
    Just state ->
      if mstate == Just state
      then sleep 20 >> coprWatchBuild bid mstate
      else do
        logMsg $ "Build" +-+ show bid +-+ state
        case state of
          "succeeded" -> return True
          "skipped" -> return True
          "canceled" -> return False
          "failed" -> return False
          _ -> sleep 1 >> coprWatchBuild bid (Just state)
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
                          taskStatus :: String,
                          taskVerRel :: String
                         }

coprMonitorPackages :: String -> String -> IO [CoprPackage]
coprMonitorPackages user proj = do
  res <- coprMonitorProject fedoraCopr user proj []
  return $ mapMaybe pkgResults $ lookupKey' "packages" res
  where
    pkgResults :: Object -> Maybe CoprPackage
    pkgResults obj = do
      name <- lookupKey "name" obj
      chroots <- map (first toText) . M.toList <$> lookupKey "chroots" obj
      return (Package name, mapMaybe chrootResults chroots)
#if !MIN_VERSION_aeson(2,0,0)
      where toText = id
#endif

    chrootResults :: (T.Text,Object) -> Maybe CoprTask
    chrootResults (chroot, obj) = do
      state <- lookupKey "state" obj
      version <- lookupKey "pkg_version" obj
      return $ CoprTask (readChroot (T.unpack chroot)) state version

-- code from copr-tool
coprMonitor :: String -> String -> IO ()
coprMonitor user proj =
  coprMonitorPackages user proj >>=
  mapM_ printPkgRes
  where
    printPkgRes (pkg,chroots) = do
      putStrLn $ "# " <> unPackage pkg
      mapM_ printChootResult chroots
      putStrLn ""

    printChootResult :: CoprTask -> IO ()
    printChootResult (CoprTask chr status version) =
      putStrLn $ showChroot chr ++ ":" +-+ status +-+ version
