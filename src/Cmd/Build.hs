{-# LANGUAGE OverloadedStrings #-}

module Cmd.Build (
  buildCmd,
  BuildOpts(..),
  UpdateType(..),
  ) where

import Common
import Common.System

import Data.Char (isDigit)
import Fedora.Bodhi hiding (bodhiUpdate)

import Bodhi
import Bugzilla
import Branches
import Cmd.Merge
import Git
import Krb
import Koji
import Package
import Prompt

data BuildOpts = BuildOpts
  { buildoptMerge :: Maybe Bool
  , buildoptNoFailFast :: Bool
  , buildoptTarget :: Maybe String
  , buildoptOverride :: Bool
  , buildoptWaitrepo :: Maybe Bool
  , buildoptDryrun :: Bool
  , buildoptUpdateType :: Maybe UpdateType
  , buildoptUseChangelog :: Bool
  , buildoptByPackage :: Bool
  }

-- FIXME --sort
-- FIXME --add-to-update nvr
-- FIXME --rpmlint (only run for rawhide?)
-- FIXME support --wait-build=NVR
-- FIXME build from ref
-- FIXME provide direct link to failed task/build.log
-- FIXME --auto-override for deps in testing
-- FIXME -B fails to find new branches (fixed?)
-- FIXME --ignore-dirty??
-- FIXME disallow override for autoupdate?
buildCmd :: BuildOpts -> (BranchesReq, [String]) -> IO ()
buildCmd opts (breq, pkgs) = do
  let singleBrnch = if isJust (buildoptTarget opts)
                    then ZeroOrOne
                    else AnyNumber
  let mlastOfPkgs = if length pkgs > 1
                    then Just (Package (last pkgs))
                    else Nothing
  if not (buildoptByPackage opts) && breq /= Branches [] && length pkgs > 1
    then do
    brs <- listOfBranches True True breq
    forM_ brs $ \br ->
      withPackageByBranches (Just False) cleanGitFetchActive singleBrnch (buildBranch mlastOfPkgs opts) (Branches [br], pkgs)
    else
    withPackageByBranches (Just False) cleanGitFetchActive singleBrnch (buildBranch mlastOfPkgs opts) (breq, pkgs)

-- FIXME what if untracked files
buildBranch :: Maybe Package -> BuildOpts -> Package -> AnyBranch -> IO ()
buildBranch _ _ _ (OtherBranch _) =
  error' "build only defined for release branches"
buildBranch mlastpkg opts pkg rbr@(RelBranch br) = do
  gitSwitchBranch rbr
  gitMergeOrigin br
  newrepo <- initialPkgRepo
  tty <- isTty
  (ancestor,unmerged) <- mergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    case buildoptMerge opts of
      Just False -> return False
      Just True -> mergeBranch True True (ancestor,unmerged) br >> return True
      Nothing ->
        if newrepo || tty
        then mergeBranch True False (ancestor,unmerged) br >> return True
        else return False
  let spec = packageSpec pkg
  checkForSpecFile spec
  checkSourcesMatch spec
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  nvr <- pkgNameVerRel' br spec
  putStrLn ""
  mpush <-
    if null unpushed then return Nothing
    else do
      when (not merged || br == Rawhide) $ do
        putStrLn $ nvr ++ "\n"
        putStrLn "Local commits:"
        mapM_ putStrLn unpushed
        putStrLn ""
      -- see mergeBranch for: unmerged == 1 (774b5890)
      if tty && (not merged || (newrepo && ancestor && length unmerged == 1))
        then do
        refPrompt unpushed $ "Press Enter to push and build" ++ (if length unpushed > 1 then "; or give a ref to push" else "") ++ (if not newrepo then "; or 'no' to skip pushing" else "")
        else return $ Just Nothing
  let dryrun = buildoptDryrun opts
  buildstatus <- maybeTimeout 30 $ kojiBuildStatus nvr
  let mtarget = buildoptTarget opts
      target = fromMaybe (branchTarget br) mtarget
      mwaitrepo = buildoptWaitrepo opts
      override = buildoptOverride opts
  case buildstatus of
    Just BuildComplete -> do
      putStrLn $ nvr ++ " is already built"
      when (isJust mpush) $
        error' "Please bump the spec file"
      when (br /= Rawhide && isNothing mtarget) $ do
        tags <- maybeTimeout 30 $ kojiNVRTags nvr
        autoupdate <- checkAutoBodhiUpdate br
        unless autoupdate $ do
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-updates-pending", show br ++ "-updates-testing", show br ++ "-updates-testing-pending"]) $ do
            mbug <- bzReviewAnon
            bodhiUpdate dryrun mbug spec nvr
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
            when override $
            bodhiCreateOverride dryrun Nothing nvr
        when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
          when ((override && mwaitrepo /= Just False) ||
                (autoupdate && mwaitrepo == Just True)) $
            kojiWaitRepo dryrun target nvr
    Just BuildBuilding -> do
      putStrLn $ nvr ++ " is already building"
      when (isJust mpush) $
        error' "Please bump the spec file"
      whenJustM (kojiGetBuildTaskID fedoraHub nvr) kojiWatchTask
      -- FIXME do override
    _ -> do
      mbuildref <- case mpush of
        Nothing -> Just <$> git "show-ref" ["--hash", "origin/" ++ show br]
        _ -> return $ join mpush
      opentasks <- kojiOpenTasks pkg mbuildref target
      case opentasks of
        [task] -> do
          putStrLn $ nvr ++ " task " ++ displayID task ++ " is already open"
          when (isJust mpush) $
            error' "Please bump the spec file"
          kojiWatchTask task
        (_:_) -> error' $ show (length opentasks) ++ " open " ++ unPackage pkg ++ " tasks already!"
        [] -> do
          let tag = fromMaybe (branchDestTag br) mtarget
          mlatest <- kojiLatestNVR tag $ unPackage pkg
          if equivNVR nvr (fromMaybe "" mlatest)
            then putStrLn $ nvr ++ " is already latest" ++ if Just nvr /= mlatest then " (modulo disttag)" else ""
            else do
            when (null unpushed || merged && br /= Rawhide) $ do
              putStrLn $ nvr ++ "\n"
            firstBuild <- do
              mtestingRepo <- bodhiTestingRepo br
              case mtestingRepo of
                Nothing -> return $ isNothing mlatest
                Just testing -> do
                  mnewest <- kojiLatestNVR testing $ unPackage pkg
                  case mnewest of
                    Nothing -> return $ isNothing mlatest
                    Just newest -> do
                      newestTags <- kojiNVRTags newest
                      unless (any (`elem` newestTags) [show br, show br ++ "-updates", show br ++ "-updates-pending"]) $ do
                        putStrLn $ "Warning: " ++ newest ++ " still in testing?"
                        prompt_ "Press Enter to continue"
                      return False
            unless dryrun krbTicket
            whenJust mpush $ \ mref ->
              unless dryrun $
              gitPushSilent $ fmap (++ ":" ++ show br) mref
            unlessM (null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")) $
              when (mpush == Just Nothing && not dryrun) $
              error' "Unpushed changes remain"
            unlessM isGitDirClean $
              error' "local changes remain (dirty)"
            -- FIXME parse build output
            unless dryrun $
              kojiBuildBranch target pkg mbuildref ["--fail-fast" | not (buildoptNoFailFast opts)]
            mBugSess <-
              if firstBuild
              then bzReviewSession
              else return Nothing
            autoupdate <- checkAutoBodhiUpdate br
            if autoupdate
              then whenJust mBugSess $
                   \ (bid,session) -> putBugBuild dryrun session bid nvr
              else do
              when (isNothing mtarget) $ do
                -- FIXME diff previous changelog?
                bodhiUpdate dryrun (fmap fst mBugSess) spec nvr
                -- FIXME prompt for override note
                when override $
                  bodhiCreateOverride dryrun Nothing nvr
            when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
              when ((override && mwaitrepo /= Just False) ||
                    (autoupdate && mwaitrepo == Just True)) $
              kojiWaitRepo dryrun target nvr
  where
    bodhiUpdate :: Bool -> Maybe BugId -> FilePath -> String -> IO ()
    bodhiUpdate dryrun mreview spec nvr = do
      changelog <- if isJust mreview
                   then getSummaryURL spec
                   else if buildoptUseChangelog opts
                        then cleanChangelog spec
                        else changeLogPrompt (Just "update") spec
      let cbugs = mapMaybe extractBugReference $ lines changelog
          bugs = let bids = [show rev | Just rev <- [mreview]] ++ cbugs in
            if null bids then [] else ["--bugs", intercalate "," bids]
      -- FIXME also query for open existing bugs
      case buildoptUpdateType opts of
        Nothing -> return ()
        Just updateType -> do
          putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
          unless dryrun $ do
            -- use cmdLog to debug, but notes are not quoted
            cmd_ "bodhi" (["updates", "new", "--type", if isJust mreview then "newpackage" else show updateType, "--request", "testing", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
            updatequery <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" nvr]
            case updatequery of
              [] -> do
                putStrLn "bodhi submission failed"
                prompt_ "Press Enter to resubmit to Bodhi"
                bodhiUpdate dryrun mreview spec nvr
              [update] -> case lookupKey "url" update of
                Nothing -> error' "Update created but no url"
                Just uri -> putStrLn uri
              _ -> error' $ "impossible happened: more than one update found for " ++ nvr

    extractBugReference :: String -> Maybe String
    extractBugReference clog =
      let rest = dropWhile (/= '#') clog in
        if null rest then Nothing
        else let bid = takeWhile isDigit $ tail rest in
          if null bid then Nothing else Just bid
