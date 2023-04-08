{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Status (
  statusCmd,
  unpushedCmd
  )
where

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

-- FIXME instead --pull?
-- FIXME --pending
-- FIXME handle not cloned (remote only)
-- FIXME silence fetching of new branches? (for --reviews etc)
statusCmd :: Bool -> Bool -> (BranchesReq,[String]) -> IO ()
statusCmd nofetch reviews (breq, pkgs) = do
  reviewpkgs <-
    if reviews
    then map reviewBugToPackage <$> listReviewsAll True ReviewRepoCreated
    else return []
  -- FIXME dirty not okay for multiple branches?
  withPackagesByBranches HeaderMay False (if nofetch then dirtyGit else dirtyGitFetch) AnyNumber statusBranch (breq, pkgs ++ reviewpkgs)
  where
    -- FIXME note dirty when local changes
    statusBranch :: Package -> AnyBranch -> IO ()
    statusBranch _ (OtherBranch _) =
      error' "status currently only defined for release branches"
    statusBranch pkg rbr@(RelBranch br) = do
      brExists <- checkIfRemoteBranchExists rbr
      if not brExists
        then do
        name <- getDirectoryName
        putStrLn $ name ++ " has no branch " ++ show br
        else do
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
              whenJustM (gitShortLog1 Nothing) $ putStrLn . showCommit
            Just nvr -> do
              -- unless (br == Rawhide) $ do
              --   newerBr <- newerBranch br <$> getFedoraBranches
              --   ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", show newerBr]
              --   when ancestor $ do
              --     unmerged <- gitOneLineLog $ "HEAD..origin/" ++ show newerBr
              --     unless (null unmerged) $ do
              --       putStrLn $ "Newer commits in " ++ show newerBr ++ ":"
              --       mapM_ putStrLn unmerged
              munpushed <- gitShortLog1 $ Just $ "origin/" ++ show br ++ "..HEAD"
              case munpushed of
                Nothing -> do
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
                      if null tags
                        then do
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
                      putNewLn
                Just unpushed ->
                  let prefix =
                        let pref =
                              (if length pkgs > 1 then unPackage pkg else "") +-+
                              case breq of
                                Branches brs | length brs <= 1 -> ""
                                _ -> show br
                        in if null pref then "" else pref ++ ":"
                  in putStrLn $ prefix +-+ showCommit unpushed
      where
        isStable :: [String] -> Bool
        isStable = not . all ("-testing" `isSuffixOf`)

        putAge :: NominalDiffTime -> IO ()
        putAge diff = do
          -- FIXME time-1.10 has formatTime of NominalDiffTime
          let (days,nomRest) = diff `divMod'` nominalDay :: (Int,NominalDiffTime)
              nominalHour = 3600 :: NominalDiffTime
              hours = nomRest `div'` nominalHour :: Int
          putStr $ " " ++ plural days "day" +-+ plural hours "hour"

#if !MIN_VERSION_time(1,8,0)
        nominalDay = 3600 * 24 :: NominalDiffTime
#endif


unpushedCmd :: Bool -> (BranchesReq,[String]) -> IO ()
unpushedCmd latest (breq, pkgs) =
  -- FIXME dirty not okay for multiple branches?
  withPackagesByBranches (if latest then HeaderMay else HeaderMust) False dirtyGit AnyNumber unpushedBranch (breq, pkgs)
  where
    -- FIXME note dirty when local changes
    unpushedBranch :: Package -> AnyBranch -> IO ()
    unpushedBranch _ (OtherBranch _) =
      error' "status currently only defined for release branches"
    unpushedBranch pkg rbr@(RelBranch br) = do
      brExists <- checkIfRemoteBranchExists rbr
      if not brExists
        then do
        name <- getDirectoryName
        putStrLn $ name ++ " has no branch " ++ show br
        else do
        gitSwitchBranch rbr
        let spec = packageSpec pkg
            prefix =
              let pref =
                    (if length pkgs > 1 then unPackage pkg else "") +-+
                    case breq of
                      Branches brs | length brs <= 1 -> ""
                      _ -> show br
              in if null pref then "" else pref ++ ":"
        ifM (notM (doesFileExist spec))
          (ifM initialPkgRepo
            (putStrLn $ prefix +-+ "initial repo")
            (unlessM (doesFileExist "dead.package") $
             putStrLn $ "missing " ++ spec)) $
          do
          whenM (isNothing <$> pkgNameVerRel br spec) $ do
            putStrLn "undefined NVR!\n"
            putStr "HEAD "
          unpushed <- gitShortLogN (if latest then Just 1 else Nothing) $
                      Just $ "origin/" ++ show br ++ "..HEAD"
          if latest
            then whenJust (listToMaybe unpushed) $ putCommit prefix
            else mapM_ (putCommit prefix) unpushed

    putCommit prefix = putStrLn . (prefix +-+) . showCommit
