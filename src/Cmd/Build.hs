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
  { buildoptMerge :: Bool
  , buildoptNoFailFast :: Bool
  , buildoptTarget :: Maybe String
  , buildoptOverride :: Bool
  , buildoptDryrun :: Bool
  , buildoptUpdateType :: Maybe UpdateType
  }

-- FIXME --add-to-update nvr
-- FIXME vertical vs horizontal builds (ie by package or branch)
-- FIXME --rpmlint (only run for master?)
-- FIXME support --wait-build=NVR
-- FIXME provide direct link to failed task/build.log
-- FIXME default behaviour for build in pkg dir: all branches or current?
buildCmd :: BuildOpts -> Maybe BranchOpts -> [String] -> IO ()
buildCmd opts mbrnchopts args = do
  let singleBrnch = if isJust (buildoptTarget opts)
                    then ZeroOrOne
                    else AnyNumber
  (brs,pkgs) <- splitBranchesPkgs True mbrnchopts True args
  let morethan1 = length pkgs > 1
  withPackageByBranches' (Just False) cleanGitFetchActive mbrnchopts singleBrnch (buildBranch morethan1 opts) (brs,pkgs)

-- FIXME what if untracked files
buildBranch :: Bool -> BuildOpts -> Package -> AnyBranch -> IO ()
buildBranch _ _ _ (OtherBranch _) =
  error' "build only defined for release branches"
buildBranch morethan1 opts pkg rbr@(RelBranch br) = do
  putPkgAnyBrnchHdr pkg rbr
  gitSwitchBranch rbr
  gitMergeOrigin rbr
  newrepo <- initialPkgRepo
  tty <- isTty
  unmerged <- mergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    if notNull unmerged && (buildoptMerge opts || newrepo || tty)
      then mergeBranch True unmerged br >> return True
      else return False
  let spec = packageSpec pkg
  checkForSpecFile spec
  checkSourcesMatch spec
  nvr <- pkgNameVerRel' br spec
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $
    putStrLn $ "\n" ++ nvr ++ "\n"
  when (not merged || br == Master) $
    unless (null unpushed) $ do
      putStrLn "Local commits:"
      mapM_ (putStrLn . simplifyCommitLog) unpushed
  mpush <-
    if null unpushed then return Nothing
    else
      -- see mergeBranch for: unmerged == 1 (774b5890)
      if tty && (not merged || (newrepo && length unmerged == 1))
      then refPrompt unpushed $ "Press Enter to push and build" ++ (if length unpushed > 1 then "; or give a ref to push" else "") ++ (if not newrepo then "; or 'no' to skip pushing" else "")
      else return $ Just Nothing
  let dryrun = buildoptDryrun opts
  buildstatus <- kojiBuildStatus nvr
  let mtarget = buildoptTarget opts
      target = fromMaybe (branchTarget br) mtarget
  case buildstatus of
    Just BuildComplete -> do
      putStrLn $ nvr ++ " is already built"
      when (isJust mpush) $
        error' "Please bump the spec file"
      when (br /= Master && isNothing mtarget) $ do
        mtags <- kojiNVRTags nvr
        case mtags of
          Nothing -> error' $ nvr ++ " is untagged"
          Just tags ->
            unless dryrun $
            unlessM (checkAutoBodhiUpdate br) $ do
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-updates-pending", show br ++ "-updates-testing", show br ++ "-updates-testing-pending"]) $ do
              mBugSess <- do
                (mbid, session) <- bzReviewSession
                return $ case mbid of
                  Just bid -> Just (bid,session)
                  Nothing -> Nothing
              bodhiUpdate (fmap fst mBugSess) spec nvr
            unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
              when (buildoptOverride opts) $
              bodhiCreateOverride nvr
            when morethan1 $ do
              autoupdate <- checkAutoBodhiUpdate br
              when (buildoptOverride opts || autoupdate) $
                kojiWaitRepo target nvr
    Just BuildBuilding -> do
      putStrLn $ nvr ++ " is already building"
      when (isJust mpush) $
        error' "Please bump the spec file"
      whenJustM (kojiGetBuildTaskID fedoraHub nvr) kojiWatchTask
      -- FIXME do override
    _ -> do
      mbuildref <- case mpush of
        Nothing -> Just <$> git "show-ref" ["--hash", "origin" </> show br]
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
            then error' $ nvr ++ " is already latest" ++ if Just nvr /= mlatest then " (modulo disttag)" else ""
            else do
            unless dryrun krbTicket
            whenJust mpush $ \ mref ->
              unless dryrun $
              gitPushSilent $ fmap (++ ":" ++ show br) mref
            unlessM (null <$> gitShortLog ("origin" </> show br ++ "..HEAD")) $
              when (mpush == Just Nothing) $
              error' "Unpushed changes remain"
            unlessM isGitDirClean $
              error' "local changes remain (dirty)"
            -- FIXME parse build output
            unless dryrun $ do
              kojiBuildBranch target pkg mbuildref ["--fail-fast"]
              mBugSess <-
                if isNothing mlatest
                then do
                (mbid, session) <- bzReviewSession
                return $ case mbid of
                  Just bid -> Just (bid,session)
                  Nothing -> Nothing
                else return Nothing
              autoupdate <- checkAutoBodhiUpdate br
              if autoupdate
                then whenJust mBugSess $
                     \ (bid,session) -> putBugBuild session bid nvr
                else do
                when (isNothing mtarget) $ do
                  -- FIXME diff previous changelog?
                  bodhiUpdate (fmap fst mBugSess) spec nvr
                  -- FIXME prompt for override note
                  when (buildoptOverride opts) $
                    bodhiCreateOverride nvr
              when morethan1 $
                when (buildoptOverride opts || autoupdate) $
                kojiWaitRepo target nvr
  where
    bodhiUpdate :: Maybe BugId -> FilePath -> String -> IO ()
    bodhiUpdate mreview spec nvr = do
      changelog <- if isJust mreview
                   then getSummaryURL spec
                   else getChangeLog spec
      let cbugs = mapMaybe extractBugReference $ lines changelog
          bugs = let bids = [show rev | Just rev <- [mreview]] ++ cbugs in
            if null bids then [] else ["--bugs", intercalate "," bids]
      -- FIXME also query for open existing bugs
      case buildoptUpdateType opts of
        Nothing -> return ()
        Just updateType -> do
          putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
          cmd_ "bodhi" (["updates", "new", "--type", if isJust mreview then "newpackage" else show updateType, "--request", "testing", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
          updatequery <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" nvr]
          case updatequery of
            [] -> do
              putStrLn "bodhi submission failed"
              prompt_ "Press Enter to resubmit to Bodhi"
              bodhiUpdate mreview spec nvr
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
