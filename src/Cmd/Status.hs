{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Status (
  statusCmd
  )
where

import Common
import Common.System

import Data.Fixed
import Data.Time.Clock
import Data.Time.LocalTime
import Distribution.Fedora.Branch (branchDestTag)
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
statusCmd :: Bool -> Bool -> Bool -> (BranchesReq,[String]) -> IO ()
statusCmd nofetch reviews latestcommit (breq, pkgs) = do
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
    statusBranch pkg rbr@(RelBranch br) =
      if latestcommit
      then do
        putStr $ unPackage pkg ++ ": "
        whenJustM (gitShortLog1 Nothing) $ putStrLn . showCommit
      else do
        brExists <- checkIfRemoteBranchExists rbr
        if not brExists
          then do
          name <- getDirectoryName
          putStrLn $ name +-+ "has no branch" +-+ showBranch br
          else do
          gitSwitchBranch rbr
          let spec = packageSpec pkg
          exists <- doesFileExist spec
          if not exists
            then
            ifM initialPkgRepo
            (putStrLn $ showBranch br ++ ": initial repo")
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
                munpushed <- gitShortLog1 $ Just $ "origin/" ++ showBranch br ++ "..HEAD"
                hub <- getKojiProfileHub
                case munpushed of
                  Nothing -> do
                    mbuild <- kojiGetBuildID hub (showNVR nvr)
                    case mbuild of
                      Nothing -> do
                        destTag <- branchDestTag br
                        mlatest <- kojiLatestNVR destTag (unPackage pkg)
                        case mlatest of
                          Nothing -> putStrLn $ "new" +-+ showNVR nvr
                          Just latest ->
                            putStrLn $ if equivNVR nvr mlatest then showNVR latest +-+ "is latest modulo disttag" else showNVR latest +-+ "->\n" ++ showNVR nvr
                      Just buildid -> do
                        tags <- kojiBuildTags hub (buildIDInfo buildid)
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
                          -- FIXME use stable_tag
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
                                  _ -> showBranch br
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
