{-# LANGUAGE CPP #-}

module Cmd.Status (statusCmd) where

import Common
import Common.System

import Data.Fixed
import Data.Time.Clock
import Data.Time.LocalTime
import Fedora.Bodhi

import Bugzilla
import Branches
import Git
import Koji
import ListReviews
import Package

-- FIXME add --no-pull?
-- FIXME --pending
-- FIXME handle not cloned (remote only)
-- FIXME silence fetching of new branches? (for --reviews etc)
statusCmd :: Bool -> (Branches,[String]) -> IO ()
statusCmd reviews (brnchs,pkgs) = do
  reviewpkgs <- if reviews then
    map reviewBugToPackage <$> listReviews' True ReviewRepoCreated
    else return []
  -- FIXME is dirty okay for some branches?
  withPackageByBranches True dirtyGitFetch statusBranch
    (brnchs, reviewpkgs ++ pkgs)

-- FIXME note dirty when local changes
statusBranch :: Package -> AnyBranch -> IO ()
statusBranch _ (OtherBranch _) =
  error' "status currently only defined for release branches"
statusBranch pkg rbr@(RelBranch br) = do
  gitSwitchBranch rbr
  let spec = packageSpec pkg
  ifM (notM (doesFileExist spec))
    (ifM initialPkgRepo
      (putStrLn $ show br ++ ": initial repo")
      (putStrLn $ "missing " ++ spec)) $
    do
    mnvr <- pkgNameVerRel br spec
    case mnvr of
      Nothing -> do
        putStrLn "undefined NVR!\n"
        putStr "HEAD "
        gitShortLog1 Nothing >>= putStrLn . simplifyCommitLog
      Just nvr -> do
        -- unless (br == Master) $ do
        --   newerBr <- newerBranch br <$> getFedoraBranches
        --   ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", show newerBr]
        --   when ancestor $ do
        --     unmerged <- gitShortLog $ "HEAD.." ++ show newerBr
        --     unless (null unmerged) $ do
        --       putStrLn $ "Newer commits in " ++ show newerBr ++ ":"
        --       mapM_ (putStrLn . simplifyCommitLog) unmerged
        unpushed <- gitShortLog1 $ Just $ "origin/" ++ show br ++ "..HEAD"
        if null unpushed then do
          mbuild <- kojiGetBuildID fedoraHub nvr
          case mbuild of
            Nothing -> do
              mlatest <- kojiLatestNVR (branchDestTag br) (unPackage pkg)
              case mlatest of
                Nothing -> putStrLn $ "new " ++ nvr
                Just latest ->
                  putStrLn $ if equivNVR nvr latest then latest ++ " is latest modulo disttag" else (if null latest then "new " else (head . words) latest ++ " ->\n") ++ nvr
            Just buildid -> do
              tags <- kojiBuildTags fedoraHub (buildIDInfo buildid)
              if null tags then do
                mstatus <- kojiBuildStatus nvr
                -- FIXME show pending archs building
                whenJust mstatus $ \ status ->
                  -- FIXME better Show BuildStatus
                  putStr $ nvr ++ " (" ++ show status ++ ")"
              else do
                -- FIXME hide testing if ga/stable
                putStr $ nvr ++ " (" ++ unwords tags ++ ")"
                unless (isStable tags) $ do
                  updates <- bodhiUpdates
                             [makeItem "display_user" "0",
                              makeItem "builds" nvr]
                  case updates of
                    [] -> putStrLn "No update found"
                    [update] -> do
                      -- FIXME could show minus time left using stable_days?
                      let msince = lookupKey "date_testing" update :: Maybe LocalTime
                      case msince of
                        Nothing -> return ()
                        Just date -> do
                          let since = localTimeToUTC utc date
                          current <- getCurrentTime
                          let diff = diffUTCTime current since
                          putAge diff
                    _ -> putStrLn "More than one update found!"
              putStrLn ""
          else putStrLn $ show br ++ ": " ++ simplifyCommitLog unpushed
  where
    isStable :: [String] -> Bool
    isStable = not . all ("-testing" `isSuffixOf`)

    putAge :: NominalDiffTime -> IO ()
    putAge diff = do
      -- FIXME time-1.10 has formatTime of NominalDiffTime
      let (days,nomRest) = diff `divMod'` nominalDay :: (Int,NominalDiffTime)
          nominalHour = 3600 :: NominalDiffTime
          hours = nomRest `div'` nominalHour :: Int
      putStr $ " " ++ showUnits days "day" +-+ showUnits hours "hour"

    showUnits :: Int -> String -> String
    showUnits i uni = show i ++ " " ++ uni ++
                      if i == 1 then "" else "s"

#if (defined(MIN_VERSION_time) && MIN_VERSION_time(1,8,0))
#else
    nominalDay = 3600 * 24 :: NominalDiffTime
#endif
