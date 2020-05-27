module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiLatestNVR,
  kojiScratchUrl,
  buildIDInfo,
  BuildState(..),
  kojiBuild,
  kojiBuildBranch
  ) where

import Data.Char (isDigit)

import Fedora.Koji

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
    else Just <$> kojiScratchBuild "rawhide" srpm

kojiScratchBuild :: String -> FilePath -> IO String
kojiScratchBuild target srpm =
  kojiBuild target ["--scratch", "--no-rebuild-srpm", srpm]

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

    takeWhileEnd :: (a -> Bool) -> [a] -> [a]
    takeWhileEnd p = reverse . takeWhile p . reverse

kojiBuildBranch :: String -> String -> [String] -> IO ()
kojiBuildBranch target pkg args = do
  commit <- git "rev-parse" ["HEAD"]
  let giturl = "git+https://src.fedoraproject.org/rpms" </> pkg ++ ".git#" ++ commit
  -- FIXME --target
  void $ kojiBuild target $ args ++ ["--fail-fast", giturl]
