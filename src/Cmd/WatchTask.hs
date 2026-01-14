module Cmd.WatchTask (
  watchTaskCmd
  )
where

import Data.Maybe (mapMaybe)
import Data.RPM.NVR (showNVR)
import Distribution.Koji (displayID, TaskID(TaskId))
import Numeric.Natural
import Safe (headMay)
import SimpleCmd (cmd_, cmdLines, error', (+-+))

import Branches (getReleaseBranch)
-- import Common.System
import Git (isPkgGitRepo)
import Koji (getKojiProfileHub, kojiGetBuildTaskID, kojiWatchTask)
import Package (getPackageName, pkgNameVerRel', packageSpec)
-- import Types

-- FIXME support watching buildArch
watchTaskCmd :: Bool -> Maybe Natural -> IO ()
watchTaskCmd progress mtaskid = do
  taskid <-
    case mtaskid of
      Just tid -> return $ TaskId $ fromIntegral tid
      Nothing -> do
        distgit <- isPkgGitRepo
        if distgit
          then do
          pkg <- getPackageName "."
          let spec = packageSpec pkg
          br <- getReleaseBranch
          nvr <- pkgNameVerRel' br spec
          hub <- getKojiProfileHub
          mbuild <- kojiGetBuildTaskID hub (showNVR nvr)
          case mbuild of
            Nothing -> error' $ "Could not find task for" +-+ showNVR nvr
            Just tid -> return tid
          else do
          opentasks <- cmdLines "koji" ["list-tasks", "--mine", "--method=build", "--quiet"]
          case mapMaybe (headMay . words) opentasks of
            [] -> error' "you have no open tasks"
            [tid] -> return $ TaskId $ read tid
            _ -> do
              mapM_ putStrLn opentasks
              error' "Specify a build taskid"
  if progress
    then cmd_ "koji-tool" ["progress", displayID taskid]
    else kojiWatchTask taskid
