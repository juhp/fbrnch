module Koji where

import Common

import SimpleCmd

import Krb

kojiBuildTags :: String -> IO (Maybe [String])
kojiBuildTags nvr =
  fmap words <$> cmdMaybe "koji" ["list-tags", "--build", nvr]

data KojiBuildStatus = COMPLETE | FAILED | BUILDING | NoBuild
  deriving (Eq, Read, Show)

kojiBuildStatus :: String -> IO KojiBuildStatus
kojiBuildStatus nvr = do
  mout <- cmdMaybe "koji" ["list-builds", "--quiet", "--buildid=" ++ nvr]
  case mout of
    Nothing -> return NoBuild
    Just out -> (return . read . last . words) out

kojiScratchUrl :: Bool -> String -> IO (Maybe String)
kojiScratchUrl noscratch srpm =
    if noscratch
    then return Nothing
    else Just <$> kojiScratchBuild False srpm

data TaskState = TaskOpen | TaskFailed | TaskClosed | TaskFree

kojiScratchBuild :: Bool -> FilePath -> IO String
kojiScratchBuild failfast srpm = do
  krbTicket
  out <- cmd "koji" $ ["build", "--scratch", "--nowait"] ++ ["--fail-fast" | failfast] ++ ["rawhide", srpm]
  putStrLn out
  let kojiurl = last $ words out
      task = takeWhileEnd (/= '=') kojiurl
  okay <- kojiWatchTask task
  if not okay
    then error' "scratch build failed"
    else return kojiurl
  where
    kojiWatchTask :: String -> IO Bool
    kojiWatchTask task = do
      res <- cmdBool "koji" ["watch-task", task]
      if res then return True
        else do
        ti <- kojiTaskInfo
        case ti of
          TaskClosed -> return True
          TaskFailed -> error "Task failed!"
          _ -> kojiWatchTask task
          where
            kojiTaskInfo :: IO TaskState
            kojiTaskInfo = do
              info <- cmdLines "koji" ["taskinfo", task]
              let state = removeStrictPrefix "State: " <$> filter ("State: " `isPrefixOf`) info
              return $
                case state of
                  ["open"] -> TaskOpen
                  ["failed"] -> TaskFailed
                  ["closed"] -> TaskClosed
                  ["free"] -> TaskFree
                  _ -> error "unknown task state!"

    takeWhileEnd :: (a -> Bool) -> [a] -> [a]
    takeWhileEnd p = reverse . takeWhile p . reverse
