{-# LANGUAGE OverloadedStrings #-}

module Cmd.Build (
  buildCmd,
  BuildOpts(..),
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
  , buildoptOverride :: Maybe Int
  , buildoptWaitrepo :: Maybe Bool
  , buildoptDryrun :: Bool
  , buildSkipFetch :: Bool
  , buildoptUpdate :: (Maybe UpdateType, UpdateSeverity)
  , buildoptUseChangelog :: Bool
  , buildoptByPackage :: Bool
  }

-- FIXME check bugs before building?
-- FIXME --sidetag
-- FIXME --sort
-- FIXME --add-to-update nvr
-- FIXME --rpmlint (default for rawhide?)
-- FIXME support --wait-build=NVR
-- FIXME build from ref
-- FIXME tail of failed build.log
-- FIXME --auto-override for deps in testing
-- FIXME -B fails to find new branches (fixed?)
-- FIXME disallow override for autoupdate?
-- FIXME count remaining packages
buildCmd :: BuildOpts -> (BranchesReq, [String]) -> IO ()
buildCmd opts (breq, pkgs) = do
  let singleBrnch = if isJust (buildoptTarget opts)
                    then ZeroOrOne
                    else AnyNumber
      mlastOfPkgs = if length pkgs > 1
                    then Just (Package (last pkgs))
                    else Nothing
      gitopts =
        if buildSkipFetch opts then cleanGitActive else cleanGitFetchActive
  if not (buildoptByPackage opts) && breq /= Branches [] && length pkgs > 1
    then do
    brs <- listOfBranches True True breq
    forM_ brs $ \br ->
      withPackageByBranches (Just False) gitopts singleBrnch (buildBranch mlastOfPkgs opts) (Branches [br], pkgs)
    else
    withPackageByBranches (Just False) gitopts singleBrnch (buildBranch mlastOfPkgs opts) (breq, pkgs)

-- FIXME what if untracked files
buildBranch :: Maybe Package -> BuildOpts -> Package -> AnyBranch -> IO ()
buildBranch _ _ _ (OtherBranch _) =
  error' "build only defined for release branches"
buildBranch mlastpkg opts pkg rbr@(RelBranch br) = do
  let moverride = buildoptOverride opts
  whenJust moverride $ \days ->
    when (days < 1) $ error "override duration must be positive number of days"
  gitSwitchBranch rbr
  gitMergeOrigin br
  newrepo <- initialPkgRepo
  tty <- isTty
  (ancestor,unmerged) <- newerMergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    case buildoptMerge opts of
      Just False -> return False
      Just True -> do
        newer <- getNewerBranch br
        mergeBranch True True (ancestor,unmerged) newer br >> return True
      Nothing ->
        if ancestor && (newrepo || tty)
        then do
          newer <- getNewerBranch br
          mergeBranch True False (ancestor,unmerged) newer br >> return True
        else do
          unless (br == Rawhide) $
            putStrLn "newer branch is not ancestor"
          return False
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
  case buildstatus of
    Just BuildComplete -> do
      putStrLn $ nvr ++ " is already built"
      when (isJust mpush) $
        error' "Please bump the spec file"
      when (br /= Rawhide && isNothing mtarget) $ do
        tags <- maybeTimeout 30 $ kojiNVRTags nvr
        autoupdate <- checkAutoBodhiUpdate br
        -- FIXME update referenced bugs for autoupdate branch
        unless autoupdate $ do
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-updates-pending", show br ++ "-updates-testing", show br ++ "-updates-testing-pending"]) $ do
            mbug <- bzReviewAnon
            bodhiUpdate dryrun mbug spec nvr
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
            whenJust moverride $ \days ->
            bodhiCreateOverride dryrun (Just days) nvr
        when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
          when ((isJust moverride && mwaitrepo /= Just False) ||
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
              if firstBuild && isJust (fst (buildoptUpdate opts))
              then bzReviewSession
              else return Nothing
            autoupdate <- checkAutoBodhiUpdate br
            if autoupdate
              then whenJust mBugSess $
                   \ (bid,session) -> putBugBuild dryrun session bid nvr
              else do
              when (isNothing mtarget) $ do
                whenJust (fmap fst mBugSess) $
                  \bid -> putStr "review bug: " >> putBugId bid
                -- FIXME diff previous changelog?
                bodhiUpdate dryrun (fmap fst mBugSess) spec nvr
                -- FIXME prompt for override note
                whenJust moverride $ \days ->
                  bodhiCreateOverride dryrun (Just days) nvr
            when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
              when ((isJust moverride && mwaitrepo /= Just False) ||
                    (autoupdate && mwaitrepo == Just True)) $
              kojiWaitRepo dryrun target nvr
  where
    bodhiUpdate :: Bool -> Maybe BugId -> FilePath -> String -> IO ()
    bodhiUpdate dryrun mreview spec nvr = do
      case buildoptUpdate opts of
        (Nothing, _) -> return ()
        (Just updateType, severity) -> do
          unless dryrun $ do
            -- use cmdLog to debug, but notes are not quoted
            updatedone <-
              if updateType == TemplateUpdate
                then do
                cmd_ "fedpkg" ["update"]
                return True
                else do
                -- FIXME also query for open existing bugs
                changelog <- if isJust mreview
                             then getSummaryURL spec
                             else if buildoptUseChangelog opts
                                  then cleanChangelog spec
                                  else
                                    -- FIXME list open bugs
                                    changeLogPrompt (Just "update") spec
                if lower changelog == "no"
                  then return False
                  else do
                  let cbugs = extractBugReferences changelog
                      bugs = let bids = [show rev | Just rev <- [mreview]] ++ cbugs in
                        if null bids then [] else ["--bugs", intercalate "," bids]
                  when (isJust mreview &&
                        updateType `elem` [SecurityUpdate,BugfixUpdate]) $
                    warning "overriding update type with 'newpackage'"
                  putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
                  -- FIXME check for Bodhi URL to confirm update
                  cmd_ "bodhi" (["updates", "new", "--type", if isJust mreview then "newpackage" else show updateType, "--severity", show severity, "--request", "testing", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
                  return True
            when updatedone $ do
              -- FIXME avoid this if we know the update URL
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

    extractBugReferences :: String -> [String]
    extractBugReferences clog =
      case dropWhile (/= '#') clog of
        "" -> []
        rest ->
          case span isDigit (tail rest) of
            (ds,more) ->
              -- make sure is contemporary 7-digit bug
              (if length ds > 6 then (ds :) else id) $
              extractBugReferences more
