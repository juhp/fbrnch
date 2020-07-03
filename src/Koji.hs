module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiLatestNVR,
  kojiScratchBuild,
  kojiScratchUrl,
  buildIDInfo,
  BuildState(..),
  kojiBuild,
  kojiBuildBranch,
  kojiWaitRepo
  ) where

import Data.Char (isDigit)

import Fedora.Koji

import Branches
import Common
import Common.System
import Git
import Krb

kojiNVRTags :: String -> IO (Maybe [String])
kojiNVRTags nvr = do
  mbldid <- kojiGetBuildID nvr
  case mbldid of
    Nothing -> return Nothing
    Just bldid -> Just <$> kojiBuildTags (buildIDInfo bldid)

kojiBuildStatus :: String -> IO (Maybe BuildState)
kojiBuildStatus nvr =
  kojiGetBuildState (BuildInfoNVR nvr)

kojiLatestNVR :: String -> String -> IO (Maybe String)
kojiLatestNVR tag pkg = do
  mbld <- kojiLatestBuild tag pkg
  return $ case mbld of
             Nothing -> Nothing
             Just bld -> lookupStruct "nvr" bld

kojiScratchUrl :: Bool -> String -> IO (Maybe String)
kojiScratchUrl noscratch srpm =
    if noscratch
    then return Nothing
    else Just <$> kojiScratchBuild "rawhide" [] srpm

kojiScratchBuild :: String -> [String] -> FilePath -> IO String
kojiScratchBuild target args srpm =
  kojiBuild target $ args ++ ["--scratch", "--no-rebuild-srpm", srpm]

kojiBuild :: String -> [String] -> IO String
kojiBuild target args = do
  krbTicket
  cmd_ "date" []
  -- FIXME setTermTitle nvr
  out <- cmd "koji" $ ["build", "--nowait", target] ++ args
  putStrLn out
  let kojiurl = last $ words out
      task = read $ takeWhileEnd isDigit kojiurl
  ifM (kojiWatchTask task)
    (return kojiurl) $
    error' "scratch build failed"
  where
    -- FIXME filter/simplify output
    kojiWatchTask :: Int -> IO Bool
    kojiWatchTask task =
      ifM (cmdBool "koji" ["watch-task", show task])
      (return True) $
      do
        mst <- kojiGetTaskState (TaskId task)
        case mst of
          Just TaskClosed -> return True
          Just TaskFailed -> error "Task failed!"
          _ -> kojiWatchTask task

kojiBuildBranch :: String -> String -> [String] -> IO ()
kojiBuildBranch target pkg args = do
  commit <- git "rev-parse" ["HEAD"]
  let giturl = "git+https://src.fedoraproject.org/rpms" </> pkg ++ ".git#" ++ commit
  void $ kojiBuild target $ args ++ ["--fail-fast", giturl]

-- FIXME use koji-hs
kojiWaitRepo :: Branch -> String -> String -> IO ()
kojiWaitRepo br target nvr =
  cmd_ "koji" ["wait-repo", targetTag, "--build=" ++ nvr]
  where
    targetTag =
      if target == branchTarget br
      then target ++ "-build"
      else target
