module Cmd.Build (
  buildCmd,
  BuildOpts(..),
  scratchCmd,
  ) where

import Common
import Common.System

import Data.Char (isDigit)
import Fedora.Bodhi hiding (bodhiUpdate)
import System.IO (hIsTerminalDevice, stdin)

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
  }

-- FIXME vertical vs horizontal builds (ie by package or branch)
-- FIXME --rpmlint (only run for master?)
-- FIXME support --wait-build=NVR
-- FIXME --dry-run
buildCmd :: BuildOpts -> ([Branch],[String]) -> IO ()
buildCmd opts (brs,pkgs) = do
  when (isJust (buildoptTarget opts) && length brs > 1) $
    error' "You can only specify target with one branch"
  withPackageByBranches False True LocalBranches (buildBranch opts) (brs,pkgs)

-- FIXME what if untracked files
buildBranch :: BuildOpts -> Package -> Branch -> IO ()
buildBranch opts pkg br = do
  putPkgBrnchHdr pkg br
  gitSwitchBranch br
  gitMergeOrigin br
  newrepo <- initialPkgRepo
  tty <- hIsTerminalDevice stdin
  unmerged <- mergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    if notNull unmerged && (buildoptMerge opts || newrepo || tty)
      then mergeBranch True unmerged br >> return True
      else return False
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  when (not merged || br == Master) $
    unless (null unpushed) $ do
      putStrLn "Local commits:"
      mapM_ (putStrLn . simplifyCommitLog) unpushed
  let spec = packageSpec pkg
  checkForSpecFile spec
  -- FIXME offer merge if newer branch has commits
  unless (null unpushed) $ do
    checkSourcesMatch spec
  -- FIXME print nvr
  mpush <-
    if null unpushed then return Nothing
    else
      -- see mergeBranch for: unmerged == 1 (774b5890)
      if (tty && (not merged || (newrepo && length unmerged == 1)))
      then refPrompt unpushed $ "Press Enter to push" ++ (if length unpushed > 1 then "; or give a ref to push" else "") ++ "; or 'no' to skip pushing"
      else return $ Just Nothing
  whenJust mpush $ gitPushSilent
  nvr <- pkgNameVerRel' br spec
  -- FIXME should compare git refs
  buildstatus <- kojiBuildStatus nvr
  case buildstatus of
    Just BuildComplete -> putStrLn $ nvr ++ " is already built"
    -- FIXME wait for build?
    Just BuildBuilding -> putStrLn $ nvr ++ " is already building"
    _ -> do
      let mtarget = buildoptTarget opts
          tag = fromMaybe (branchDestTag br) mtarget
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      if dropExtension nvr == dropExtension (fromMaybe "" mlatest)
        then error' $ nvr ++ " is already latest (modulo disttag)"
        else do
        krbTicket
        unlessM (null <$> gitShortLog ("origin" </> show br ++ "..HEAD")) $
          when (mpush == Just Nothing) $
          error' "Unpushed changes remain"
        unlessM isGitDirClean $
          error' "local changes remain (dirty)"
        let  target = fromMaybe (branchTarget br) mtarget
        mbuildref <- case mpush of
          Nothing -> Just <$> git "show-ref" ["--hash", "origin" </> show br]
          _ -> return $ join mpush
        -- FIXME parse build output
        kojiBuildBranch target (unPackage pkg) mbuildref ["--fail-fast"]
        -- FIXME get bugs from changelog
        mBugSess <- if isNothing mlatest
          then do
          (mbid, session) <- bzReviewSession
          return $ case mbid of
            Just bid -> Just (bid,session)
            Nothing -> Nothing
          else return Nothing
        if br == Master
          then case mBugSess of
                 Just (bid,session) -> postBuildComment session nvr bid
                 Nothing -> return ()
          else do
          -- FIXME diff previous changelog?
          changelog <- getChangeLog spec
          bodhiUpdate (fmap fst mBugSess) changelog nvr
          -- FIXME autochain
          -- FIXME prompt for override note
          when (buildoptOverride opts) $ do
            when (br /= Master) $
              bodhiCreateOverride nvr
            kojiWaitRepo br target nvr
  where
    bodhiUpdate :: Maybe BugId -> String -> String -> IO ()
    bodhiUpdate mreview changelog nvr = do
      let cbugs = mapMaybe extractBugReference $ lines changelog
          bugs = let bids = [show rev | Just rev <- [mreview]] ++ cbugs in
            if null bids then [] else ["--bugs", intercalate "," bids]
      -- FIXME check for autocreated update (pre-updates-testing)
      -- FIXME also query for open existing bugs
      -- FIXME extract bug no(s) from changelog
      putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
      updateOK <- cmdBool "bodhi" (["updates", "new", "--type", if isJust mreview then "newpackage" else "enhancement", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
      unless updateOK $ do
        updatequery <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" nvr]
        case updatequery of
          [] -> do
            putStrLn "bodhi submission failed"
            prompt_ "Press Enter to resubmit to Bodhi"
            bodhiUpdate mreview changelog nvr
          [update] -> case lookupKey "url" update of
            Nothing -> error' "Update created but no url"
            Just uri -> putStrLn uri
          _ -> error' $ "impossible happened: more than one update found for " ++ nvr

    checkSourcesMatch :: FilePath -> IO ()
    checkSourcesMatch spec = do
      -- "^[Ss]ource[0-9]*:"
      source <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
      sources <- lines <$> readFile "sources"
      forM_ source $ \ src ->
        when (isNothing (find (src `isInfixOf`) sources)) $
        unlessM (doesFileExist (takeFileName src)) $
        -- FIXME offer to add source
        error' $ src ++ " not in sources"

    extractBugReference :: String -> Maybe String
    extractBugReference clog =
      let rest = dropWhile (/= '#') clog in
        if null rest then Nothing
        else let bid = takeWhile isDigit $ tail rest in
          if null bid then Nothing else Just bid

    bodhiCreateOverride :: String -> IO ()
    bodhiCreateOverride nvr = do
      putStrLn $ "Creating Bodhi Override for " ++ nvr ++ ":"
      ok <- cmdBool "bodhi" ["overrides", "save", "--notes", "chain building", "--duration", "7", nvr]
      unless ok $ do
        moverride <- bodhiOverride nvr
        case moverride of
          Nothing -> do
            putStrLn "bodhi override failed"
            prompt_ "Press Enter to retry"
            bodhiCreateOverride nvr
          Just obj -> print obj

-- FIXME --arches
-- FIXME --exclude-arch
-- FIXME build from a git ref
scratchCmd :: Bool -> Bool -> Maybe String -> Maybe String -> (Maybe Branch,[String]) -> IO ()
scratchCmd rebuildSrpm nofailfast march mtarget (mbr,pkgs) =
  withPackageByBranches False False NoGitRepo (scratchBuild rebuildSrpm nofailfast march mtarget) (maybeToList mbr,pkgs)

scratchBuild :: Bool -> Bool -> Maybe String -> Maybe String -> Package -> Branch -> IO ()
scratchBuild rebuildSrpm nofailfast march mtarget pkg br = do
  spec <- localBranchSpecFile pkg br
  let target = fromMaybe "rawhide" mtarget
  let args = ["--arch-override=" ++ fromJust march | isJust march] ++ ["--fail-fast" | not nofailfast] ++ ["--no-rebuild-srpm" | not rebuildSrpm]
  pkggit <- isPkgGitDir
  if pkggit
    then do
    gitSwitchBranch br
    pushed <- do
      clean <- isGitDirClean
      if clean then
        null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
        else return False
    if pushed then do
      void $ getSources spec
      kojiBuildBranch target (unPackage pkg) Nothing $ "--scratch" : args
      else srpmBuild target args spec
    else srpmBuild target args spec
  where
    srpmBuild :: FilePath -> [String] -> String -> IO ()
    srpmBuild target args spec = do
      void $ getSources spec
      void $ generateSrpm (Just br) spec >>= kojiScratchBuild target args
