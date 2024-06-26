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
        putStrLn $ name +-+ "has no branch" +-+ show br
        else do
        gitSwitchBranch rbr
        let spec = packageSpec pkg
        exists <- doesFileExist spec
        if not exists
          then
          ifM initialPkgRepo
          (putStrLn $ show br ++ ": initial repo")
          (putStrLn $ "missing" +-+ spec)
          else do
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
              --       putStrLn $ "Newer commits in" +-+ show newerBr ++ ":"
              --       mapM_ putStrLn unmerged
              munpushed <- gitShortLog1 $ Just $ "origin/" ++ show br ++ "..HEAD"
              case munpushed of
                Nothing -> do
                  mbuild <- kojiGetBuildID fedoraHub (showNVR nvr)
                  case mbuild of
                    Nothing -> do
                      mlatest <- kojiLatestNVR (branchDestTag br) (unPackage pkg)
                      case mlatest of
                        Nothing -> putStrLn $ "new" +-+ showNVR nvr
                        Just latest ->
                          putStrLn $ if equivNVR nvr mlatest then showNVR latest +-+ "is latest modulo disttag" else showNVR latest +-+ "->\n" ++ showNVR nvr
                    Just buildid -> do
                      tags <- kojiBuildTags fedoraHub (buildIDInfo buildid)
                      if null tags
                        then do
                        mstatus <- kojiBuildStatus nvr
                        -- FIXME show pending archs building
                        whenJust mstatus $ \ status ->
                          -- FIXME better Show BuildStatus
                          putStr $ showNVR nvr +-+ "(" ++ show status ++ ")"
                        else do
                        -- FIXME hide testing if ga/stable
                        putStr $ showNVR nvr +-+ "(" ++ unwords tags ++ ")"
                        unless (isStable tags) $ do
                          updates <- bodhiUpdates
                                     [makeItem "display_user" "0",
                                      makeItem "builds" (showNVR nvr)]
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


unpushedCmd :: Bool -> Bool -> (BranchesReq,[String]) -> IO ()
unpushedCmd latest bump (breq, pkgs) =
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
        putStrLn $ name +-+ "has no branch" +-+ show br
        else do
        gitSwitchBranch rbr
        let spec = packageSpec pkg
            prefix =
              let pref =
                    (if length pkgs > 1 && latest
                     then unPackage pkg else "") +-+
                    case breq of
                      Branches brs | length brs <= 1 -> ""
                      _ -> show br
              in if null pref then "" else pref ++ ":"
        haveSpec <- doesFileExist spec
        if not haveSpec
          then
          ifM initialPkgRepo
          (putStrLn $ prefix +-+ "initial repo") $
          ifM (doesFileExist "dead.package")
          (putStrLn $ prefix +-+ "dead package") $
          putStrLn $ prefix +-+ "missing" +-+ spec
          else do
          whenM (isNothing <$> pkgNameVerRel br spec) $ do
            putStrLn "undefined NVR!\n"
            putStr "HEAD "
          unpushed <- gitShortLogN (if latest then Just 1 else Nothing) $
                      Just $ "origin/" ++ show br ++ "..HEAD"
          if null unpushed
            then
            when bump $ doBump spec
            else
            if latest
            then whenJust (listToMaybe unpushed) $ putCommit prefix
            else mapM_ (putCommit prefix) unpushed

    putCommit prefix = putStrLn . (prefix +-+) . showCommit

    doBump spec = do
      checkWorkingDirClean False
      dead <- doesFileExist "dead.package"
      if dead
        then putStrLn "dead package"
        else do
        putStrLn "bumping"
        autorelease <- isAutoRelease spec
        unless autorelease $
          cmd_ "rpmdev-bumpspec" ["-c", "rebuild", spec]
        git_ "commit" $ "-a" : (if autorelease then ("--allow-empty" :) else id) ["-m", "bump release"]
