{-# LANGUAGE OverloadedStrings #-}

module Cmd.Build (
  buildCmd,
  BuildOpts(..)
  ) where

import Distribution.Fedora.Branch (branchDestTag)
import SimplePrompt (promptEnter, yesNo)

import Bodhi
import Bugzilla
import Branches
import Common
import Common.System
import Cmd.Merge
import Git
import Krb
import Koji
import Package
import RpmBuild (checkSourcesMatch)
import Types

data BuildOpts = BuildOpts
  { buildoptMerge :: Maybe Bool
  , buildoptNoFailFast :: Bool
  , buildoptSidetagTarget :: Maybe SideTagTarget
  , buildoptOverride :: Maybe Int
  , buildoptWaitrepo :: Maybe Bool
  , buildoptDryrun :: Bool
  , buildoptSkipFetch :: Bool
  , buildoptUpdate :: (Maybe UpdateType, UpdateSeverity)
  , buildoptNotes :: Maybe UpdateNotes
  , buildoptByPackage :: Bool
  , buildoptStash :: Bool
  }

-- FIXME --yes
-- FIXME merge --from
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
-- FIXME --scratch build first
-- FIXME --skip-bumps NUM
buildCmd :: BuildOpts -> (BranchesReq, [String]) -> IO ()
buildCmd opts (breq, pkgs) = do
  let singleBrnch = if isJust (buildoptSidetagTarget opts)
                    then ZeroOrOne
                    else AnyNumber
      mlastOfPkgs = if length pkgs > 1
                    then Just (Package (last pkgs))
                    else Nothing
      gitopts
        | buildoptStash opts = stashGitFetch
        | buildoptSkipFetch opts = cleanGitActive
        | otherwise = cleanGitFetchActive
  if not (buildoptByPackage opts) && breq /= Branches [] && length pkgs > 1
    then do
    brs <- listOfBranches True True breq
    forM_ brs $ \br ->
      withPackagesByBranches HeaderMay True gitopts singleBrnch (buildBranch mlastOfPkgs opts) (Branches [br], pkgs)
    else
    withPackagesByBranches HeaderMay True gitopts singleBrnch (buildBranch mlastOfPkgs opts) (breq, pkgs)

-- FIXME display existing sidetag early
-- FIXME what if untracked files
-- FIXME --merge instead of --yes confusing
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
  (ancestor,unmerged,mnewer) <- newerMergeable (unPackage pkg) br
  -- FIXME if already built or failed, also offer merge
  merged <-
    case buildoptMerge opts of
      Just False -> return False
      Just True -> do
        whenJust mnewer $ \newer ->
          mergeBranch (buildoptDryrun opts) True True False pkg (ancestor,unmerged) newer br
        return True
      Nothing ->
        if ancestor && (newrepo || tty)
        then do
          whenJust mnewer $ \newer ->
            mergeBranch (buildoptDryrun opts) True False True pkg (ancestor,unmerged) newer br
          return $ isJust mnewer
        else do
          unless (br == Rawhide) $
            whenJust mnewer $ \newer ->
            putStrLn $ showBranch newer +-+ "branch not mergeable"
          return False
  let spec = packageSpec pkg
  checkForSpecFile spec
  checkSourcesMatch pkg (RelBranch br) spec
  unpushed <- gitOneLineLog $ "origin/" ++ showBranch br ++ "..HEAD"
  nvr <- pkgNameVerRel' br spec
  putNewLn
  mpush <-
    case unpushed of
      [] -> return Nothing
      (unpd:_) -> do
        when (not merged || br == Rawhide) $ do
          -- FIXME should be printed for new package branch
          putStrLn $ showNVR nvr ++ "\n"
          putStrLn "Local commits:"
          displayCommits True unpushed
          putNewLn
        -- see mergeBranch for: unmerged == 1 (774b5890)
        if tty && (not merged || (newrepo && ancestor && length unmerged == 1))
          then refPrompt unpushed $ "Press Enter to push and build" ++ (if length unpushed > 1 then "; or give ref to push" else "") ++ (if not newrepo then "; or 'no' to skip pushing" else "")
          else return $ Just $ commitRef unpd
  let msidetagTarget = buildoptSidetagTarget opts
  target <- targetMaybeSidetag dryrun True True br msidetagTarget
  buildRun spec nvr merged mpush unpushed target msidetagTarget moverride
  where
    dryrun = buildoptDryrun opts

    buildRun spec nvr merged mpush unpushed target msidetagTarget moverride = do
      let mwaitrepo = buildoptWaitrepo opts
      buildstatus <- maybeTimeout 30 $ kojiBuildStatus nvr
      case buildstatus of
        Just BuildComplete -> do
          putStrLn $ showNVR nvr +-+ "is built"
          when (isJust mpush) $
            error' "Please bump the spec file"
          autoupdate <- checkAutoBodhiUpdate br
          if not autoupdate && isNothing msidetagTarget
            then do
            updateExists <- maybeTimeout 30 $ bodhiBuildExists nvr
            -- FIXME update referenced bugs for autoupdate branch
            unless autoupdate $ do
              if updateExists
                then putStrLn "update exists"
                else do
                mbug <- bzReviewAnon
                bodhiUpdate dryrun (buildoptUpdate opts) mbug (buildoptNotes opts) spec $ showNVR nvr
              whenJust moverride $ \days -> do
                tags <- maybeTimeout 30 $ kojiNVRTags nvr
                unless (any (`elem` tags) [showBranch br, showBranch br ++ "-updates", showBranch br ++ "-override"]) $
                  bodhiCreateOverride dryrun (Just days) nvr
            when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
              when ((isJust moverride && mwaitrepo /= Just False) ||
                    (mwaitrepo == Just True)) $
                kojiWaitRepoNVR dryrun False target nvr
            else
            when (mwaitrepo == Just True) $
            kojiWaitRepoNVR dryrun False target nvr
        Just BuildBuilding -> do
          putStrLn $ showNVR nvr +-+ "is already building"
          when (isJust mpush) $
            error' "Please bump the spec file"
          whenJustM (kojiGetBuildTaskID fedoraHub (showNVR nvr)) kojiWatchTask
          buildRun spec nvr merged mpush unpushed target msidetagTarget moverride
        _ -> do
          mbuildref <-
            case mpush of
              Nothing -> Just <$> git "show-ref" ["--hash", "origin/" ++ showBranch br]
              _ -> return mpush
          opentasks <- kojiOpenTasks pkg mbuildref target
          case opentasks of
            [task] -> do
              putStrLn $ showNVR nvr +-+ "task" +-+ displayID task +-+ "is already open"
              when (isJust mpush) $
                error' "Please bump the spec file"
              kojiWatchTask task
            (_:_) -> error' $ show (length opentasks) +-+ "open" +-+ unPackage pkg +-+ "tasks already!"
            [] -> do
              tag <-
                if target == showBranch br
                then branchDestTag br
                else return target
              mlatest <- kojiLatestNVR tag $ unPackage pkg
              if equivNVR nvr mlatest
                then putStrLn $ showNVR nvr +-+ "is already latest" +-+ if Just nvr /= mlatest then "(modulo disttag)" else ""
                else do
                when (null unpushed || merged && br /= Rawhide) $ do
                  putStrLn $ showNVR nvr ++ "\n"
                firstBuild <- do
                  mtestingtag <- bodhiTestingRepoTag br
                  case mtestingtag of
                    Nothing -> return $ isNothing mlatest
                    Just testingtag -> do
                      mnewest <- kojiLatestNVR testingtag $ unPackage pkg
                      case mnewest of
                        Nothing -> return $ isNothing mlatest
                        Just newest -> do
                          newestTags <- kojiNVRTags newest
                          unless (any (`elem` newestTags) [showBranch br, showBranch br ++ "-updates", showBranch br ++ "-updates-pending"]) $ do
                            -- FIXME print how many days left
                            putStrLn $ "Warning:" +-+ showNVR newest +-+ "still in testing?"
                            promptEnter "Press Enter to continue"
                          return False
                unless (buildoptStash opts) $
                  unlessM isGitDirClean $
                  error' "local changes remain (dirty)"
                unless dryrun krbTicket
                whenJust mpush $ \ref ->
                  unless dryrun $
                  gitPush False $ Just $ ref ++ ":" ++ showBranch br
                unlessM (null <$> gitOneLineLog ("origin/" ++ showBranch br ++ "..HEAD")) $
                  unless dryrun $ do
                  ok <- yesNo "Unpushed changes remain, continue"
                  unless ok $ error' "aborted"
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
                  when (isNothing msidetagTarget) $ do
                    whenJust (fmap fst mBugSess) $
                      \bid -> putStr "review bug: " >> putBugId bid
                    -- FIXME diff previous changelog?
                    bodhiUpdate dryrun (buildoptUpdate opts) (fmap fst mBugSess) (buildoptNotes opts) spec $ showNVR nvr
                    -- FIXME prompt for override note
                    whenJust moverride $ \days ->
                      bodhiCreateOverride dryrun (Just days) nvr
                when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
                  when ((isJust moverride && mwaitrepo /= Just False) ||
                        (mwaitrepo == Just True)) $
                  kojiWaitRepoNVR dryrun False target nvr
