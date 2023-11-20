{-# LANGUAGE OverloadedStrings #-}

module Cmd.Build (
  buildCmd,
  BuildOpts(..)
  ) where

import SimplePrompt (promptEnter, yesNo)

import Common
import Common.System

import Bodhi
import Bugzilla
import Branches
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
  , buildoptUseChangelog :: Bool
  , buildoptByPackage :: Bool
  , buildoptAllowDirty :: Bool
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
buildCmd :: BuildOpts -> (BranchesReq, [String]) -> IO ()
buildCmd opts (breq, pkgs) = do
  let singleBrnch = if isJust (buildoptSidetagTarget opts)
                    then ZeroOrOne
                    else AnyNumber
      mlastOfPkgs = if length pkgs > 1
                    then Just (Package (last pkgs))
                    else Nothing
      gitopts
        | buildoptAllowDirty opts = dirtyGitActive
        | buildoptSkipFetch opts = cleanGitActive
        | otherwise = cleanGitFetchActive
  if not (buildoptByPackage opts) && breq /= Branches [] && length pkgs > 1
    then do
    brs <- listOfBranches True True breq
    forM_ brs $ \br ->
      withPackagesByBranches HeaderMay True gitopts singleBrnch (buildBranch mlastOfPkgs opts) (Branches [br], pkgs)
    else
    withPackagesByBranches HeaderMay True gitopts singleBrnch (buildBranch mlastOfPkgs opts) (breq, pkgs)

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
  (ancestor,unmerged,mnewer) <- newerMergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    case buildoptMerge opts of
      Just False -> return False
      Just True -> do
        whenJust mnewer $ \newer ->
          mergeBranch (buildoptDryrun opts) True True False Nothing (ancestor,unmerged) newer br
        return True
      Nothing ->
        if ancestor && (newrepo || tty)
        then do
          whenJust mnewer $ \newer ->
            mergeBranch (buildoptDryrun opts) True False True Nothing (ancestor,unmerged) newer br
          return $ isJust mnewer
        else do
          unless (br == Rawhide) $
            whenJust mnewer $ \newer ->
            putStrLn $ show newer +-+ "branch not mergeable"
          return False
  let spec = packageSpec pkg
  checkForSpecFile spec
  checkSourcesMatch pkg (RelBranch br) spec
  unpushed <- gitOneLineLog $ "origin/" ++ show br ++ "..HEAD"
  nvr <- pkgNameVerRel' br spec
  putNewLn
  mpush <-
    if null unpushed
    then return Nothing
    else do
      when (not merged || br == Rawhide) $ do
        putStrLn $ nvr ++ "\n"
        putStrLn "Local commits:"
        displayCommits True unpushed
        putNewLn
      -- see mergeBranch for: unmerged == 1 (774b5890)
      if tty && (not merged || (newrepo && ancestor && length unmerged == 1))
        then refPrompt unpushed $ "Press Enter to push and build" ++ (if length unpushed > 1 then "; or give ref to push" else "") ++ (if not newrepo then "; or 'no' to skip pushing" else "")
        else return $ Just $ commitRef $ head unpushed
  let dryrun = buildoptDryrun opts
  buildstatus <- maybeTimeout 30 $ kojiBuildStatus nvr
  let msidetagTarget = buildoptSidetagTarget opts
      mwaitrepo = buildoptWaitrepo opts
  target <- targetMaybeSidetag dryrun br msidetagTarget
  case buildstatus of
    Just BuildComplete -> do
      putStrLn $ nvr +-+ "is already built"
      when (isJust mpush) $
        error' "Please bump the spec file"
      when (br /= Rawhide && isNothing msidetagTarget) $ do
        updateExists <- maybeTimeout 30 $ bodhiBuildExists nvr
        autoupdate <- checkAutoBodhiUpdate br
        -- FIXME update referenced bugs for autoupdate branch
        unless autoupdate $ do
          if updateExists
            then putStrLn "update exists"
            else do
            mbug <- bzReviewAnon
            bodhiUpdate dryrun (buildoptUpdate opts) mbug (buildoptUseChangelog opts) spec nvr
          tags <- maybeTimeout 30 $ kojiNVRTags nvr
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
            whenJust moverride $ \days ->
            bodhiCreateOverride dryrun (Just days) nvr
        when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
          when ((isJust moverride && mwaitrepo /= Just False) ||
                (autoupdate && mwaitrepo == Just True)) $
            kojiWaitRepo dryrun True True target nvr
    Just BuildBuilding -> do
      putStrLn $ nvr +-+ "is already building"
      when (isJust mpush) $
        error' "Please bump the spec file"
      whenJustM (kojiGetBuildTaskID fedoraHub nvr) kojiWatchTask
      -- FIXME do override
    _ -> do
      mbuildref <-
        case mpush of
          Nothing -> Just <$> git "show-ref" ["--hash", "origin/" ++ show br]
          _ -> return mpush
      opentasks <- kojiOpenTasks pkg mbuildref target
      case opentasks of
        [task] -> do
          putStrLn $ nvr +-+ "task" +-+ displayID task +-+ "is already open"
          when (isJust mpush) $
            error' "Please bump the spec file"
          kojiWatchTask task
        (_:_) -> error' $ show (length opentasks) +-+ "open" +-+ unPackage pkg +-+ "tasks already!"
        [] -> do
          let tag =
                if target == branchTarget br then branchDestTag br else target
          mlatest <- kojiLatestNVR tag $ unPackage pkg
          if equivNVR nvr (fromMaybe "" mlatest)
            then putStrLn $ nvr +-+ "is already latest" +-+ if Just nvr /= mlatest then "(modulo disttag)" else ""
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
                        -- FIXME print how many days left
                        putStrLn $ "Warning:" +-+ newest +-+ "still in testing?"
                        promptEnter "Press Enter to continue"
                      return False
            unless dryrun krbTicket
            whenJust mpush $ \ref ->
              unless dryrun $
              gitPush False $ Just $ ref ++ ":" ++ show br
            unlessM (null <$> gitOneLineLog ("origin/" ++ show br ++ "..HEAD")) $
              unless dryrun $ do
              ok <- yesNo "Unpushed changes remain, continue"
              unless ok $ error' "aborted"
            unless (buildoptAllowDirty opts) $
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
              when (isNothing msidetagTarget) $ do
                whenJust (fmap fst mBugSess) $
                  \bid -> putStr "review bug: " >> putBugId bid
                -- FIXME diff previous changelog?
                bodhiUpdate dryrun (buildoptUpdate opts) (fmap fst mBugSess) (buildoptUseChangelog opts) spec nvr
                -- FIXME prompt for override note
                whenJust moverride $ \days ->
                  bodhiCreateOverride dryrun (Just days) nvr
            when (isJust mlastpkg && mlastpkg /= Just pkg || mwaitrepo == Just True) $
              when ((isJust moverride && mwaitrepo /= Just False) ||
                    (autoupdate && mwaitrepo == Just True)) $
              kojiWaitRepo dryrun True True target nvr
