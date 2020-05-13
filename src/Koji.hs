module Koji (
  kojiNVRTags,
  kojiBuildStatus,
  kojiBuildTags,
  kojiGetBuildID,
  kojiLatestNVR,
  kojiScratchUrl,
  kojiScratchBuild,
  buildIDInfo,
  BuildState(..)
  ) where

import Data.Char (isDigit)

import Fedora.Koji
import SimpleCmd

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
    else Just <$> kojiScratchBuild False srpm

kojiScratchBuild :: Bool -> FilePath -> IO String
kojiScratchBuild failfast srpm = do
  krbTicket
  out <- cmd "koji" $ ["build", "--scratch", "--nowait"] ++ ["--fail-fast" | failfast] ++ ["rawhide", srpm]
  putStrLn out
  let kojiurl = last $ words out
      task = read $ takeWhileEnd isDigit kojiurl
  okay <- kojiWatchTask task
  if not okay
    then error' "scratch build failed"
    else return kojiurl
  where
    kojiWatchTask :: Int -> IO Bool
    kojiWatchTask task = do
      res <- cmdBool "koji" ["watch-task", show task]
      if res then return True
        else do
        mst <- kojiGetTaskState (TaskId task)
        case mst of
          Just TaskClosed -> return True
          Just TaskFailed -> error "Task failed!"
          _ -> kojiWatchTask task

    takeWhileEnd :: (a -> Bool) -> [a] -> [a]
    takeWhileEnd p = reverse . takeWhile p . reverse
