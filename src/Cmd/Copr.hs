{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Copr (
  BuildBy(..),
  coprCmd
  )
where

import Branches
import Common
import Common.System
import qualified Common.Text as T
import Package

import Data.Ini.Config
import Network.HTTP.Query (lookupKey, lookupKey')
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.Time.Extra (sleep)
import Web.Fedora.Copr (coprChroots, fedoraCopr)
import Web.Fedora.Copr.API (coprGetBuild)

data BuildBy = SingleBuild | ValidateByRelease | ValidateByArch | BuildByRelease
  deriving (Eq)

coprServer :: String
coprServer = "copr.fedorainfracloud.org"

-- FIXME make project optional (if same as pkg??) or configurable ?
-- FIXME repo config with a setup command?
-- FIXME interact with copr dist-git
-- FIXME parallel copr builds
coprCmd :: Bool -> Bool -> BuildBy -> [String] -> String
        -> (BranchesReq,[String]) -> IO ()
coprCmd dryrun listchroots buildBy archs project (breq, pkgs) = do
  chroots <- coprGetChroots
  if listchroots
    then mapM_ putStrLn chroots
    else
    if null pkgs then
      getPackageName "." >>= coprBuildPkg chroots
    else
      mapM_ (\ p -> withExistingDirectory p $ coprBuildPkg chroots (Package p)) pkgs
  where
    coprGetChroots = do
      username <- getUsername
      chroots <- map T.unpack <$> coprChroots coprServer username project
      when (null chroots) $
        error' $ "No chroots found for " ++ username ++ "/" ++ project
      branches <-
        case breq of
          Branches brs ->
            if null brs
            then return $ (map (releaseBranch . T.pack) . nub . map removeArch) chroots
            else listOfBranches False False breq
          _ -> listOfBranches False False breq
      let buildroots =
            reverseSort $
            if null archs
            then [chroot | chroot <- chroots, removeArch chroot `elem` map branchRelease branches]
            else [chroot | arch <- archs, br <- branches, let chroot = branchRelease br ++ "-" ++ arch, chroot `elem` chroots]
      if null buildroots
        then error' "No valid chroots"
        else return buildroots

    coprBuildPkg buildroots pkg = do
      -- FIXME check is pkg.spec
      spec <- localBranchSpecFile pkg (RelBranch Rawhide)
      -- pkg <- takeFileName <$> getCurrentDirectory
      -- hack to avoid generating srpm for dryrun
      srpm <- if not dryrun
              then generateSrpm Nothing spec -- FIXME: let distopt = ["--undefine", "dist"]
              else return spec
      case buildBy of
        SingleBuild -> coprBuild dryrun project srpm buildroots
        -- FIXME or default to secondary parallel to previous primary
        ValidateByRelease -> do
          let initialChroots =
                let primaryArch = releaseArch $ head buildroots
                in map pure $ filter (isArch primaryArch) buildroots
              remainingChroots = buildroots \\ concat initialChroots
          staggerBuilds srpm initialChroots remainingChroots
        ValidateByArch -> do
          let initialChroots =
                let newestRelease = removeArch $ head buildroots
                in map pure $ filter (newestRelease `isPrefixOf`) buildroots
              remainingChroots = buildroots \\ concat initialChroots
          staggerBuilds srpm initialChroots remainingChroots
        BuildByRelease -> do
          let initialChroots = groupBy sameRelease buildroots
              remainingChroots = buildroots \\ concat initialChroots
          staggerBuilds srpm initialChroots remainingChroots

    removeArch relarch = init $ dropWhileEnd (/= '-') relarch

    staggerBuilds srpm initialChroots remainingChroots = do
      mapM_ (coprBuild dryrun project srpm) initialChroots
      unless (null remainingChroots) $
        coprBuild dryrun project srpm remainingChroots

    releaseArch relarch = takeWhileEnd (/= '-') relarch

    isArch arch release = releaseArch release == arch

    sameRelease r1 r2 = removeArch r1 == removeArch r2

    reverseSort = reverse . sort

branchRelease :: Branch -> String
branchRelease Rawhide = "fedora-rawhide"
branchRelease (Fedora n) = "fedora-" ++ show n
branchRelease (EPEL n) = "epel-" ++ show n

--data Chroot = Chroot Release Arch

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
    then error' $ inifile ++ " not found: try https://" ++ coprServer ++ "/api"
    else do
    ini <- T.readFile inifile
    let config = parseIniFile ini iniparser
    return $ either error' record config

coprBuild :: Bool -> String -> FilePath -> [String] -> IO ()
coprBuild _ _ _ [] = error' "No chroots chosen"
coprBuild dryrun project srpm buildroots = do
  let chrootargs = mconcat [["-r", bldrt] | bldrt <- buildroots]
      buildargs = ["build", "--nowait"] ++ chrootargs ++ [project, srpm]
  putStrLn ""
  putStrLn $ unwords buildroots
  unless dryrun $ do
    output <- cmd "copr" buildargs
    putStrLn output
    let bid = read $ last $ words $ last $ lines output
    ok <- coprWatchBuild bid Nothing
    unless ok $
      error' $ "Failed: copr " ++ unwords buildargs

-- FIXME idea: Maybe Seconds to increment sleep
coprWatchBuild :: Int -> Maybe String -> IO Bool
coprWatchBuild bid mstate = do
  res <- coprGetBuild fedoraCopr bid
  case lookupKey "state" res of
    Just state ->
      if mstate == Just state
      then sleep 20 >> coprWatchBuild bid mstate
      else do
        logMsg $ "Build " ++ show bid ++ " " ++ state
        case state of
          "succeeded" -> return True
          "skipped" -> return True
          "canceled" -> return False
          "failed" -> return False
          _ -> sleep 1 >> coprWatchBuild bid (Just state)
    Nothing -> do
      let err = lookupKey' "error" res
      logMsg $ "Error: " ++ err
      return False

-- #if !MIN_VERSION_simple_cmd(0,1,4)
-- error' :: String -> a
-- #if MIN_VERSION_base(4,9,0)
-- error' = errorWithoutStackTrace
-- #else
-- error' = error
-- #endif
-- #endif
