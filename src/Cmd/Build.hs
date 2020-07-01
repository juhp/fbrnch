module Cmd.Build (
  buildCmd,
  Scratch(..)
  ) where

import Common
import Common.System

import Data.Char (isDigit)
import System.IO (hIsTerminalDevice, stdin)
import Fedora.Bodhi hiding (bodhiUpdate)

import Bugzilla
import Branches
import Cmd.Merge
import Git
import Krb
import Koji
import Package
import Prompt

data Scratch = AllArches | Arch String

-- FIXME sort packages in build dependency order (chain-build?)
-- FIXME --no-fast-fail
-- FIXME --(no-)rpmlint (only run for master?)
-- FIXME support --rebuild-srpm --wait-build=NVR --background
-- FIXME add --override
buildCmd :: Bool -> Maybe Scratch -> Maybe String -> ([Branch],[String]) -> IO ()
buildCmd merge' scratch mtarget (brs,pkgs) = do
  when (isJust mtarget && length brs > 1) $
    error' "You can only specify target with one branch"
  withPackageByBranches False LocalBranches (buildBranch merge' False scratch mtarget) (brs,pkgs)

buildBranch :: Bool -> Bool -> Maybe Scratch -> Maybe String -> Package -> Branch -> IO ()
buildBranch merge' override scratch mtarget pkg br = do
  putPkgBrnchHdr pkg br
  gitSwitchBranch br
  when (isNothing scratch) $ gitMergeOrigin br
  newrepo <- initialPkgRepo
  tty <- hIsTerminalDevice stdin
  unmerged <- mergeable br
  -- FIXME if already built or failed, also offer merge
  merged <-
    if notNull unmerged && (merge' || newrepo || tty)
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
  when (not (null unpushed) && isNothing scratch) $ do
    checkSourcesMatch spec
    -- FIXME offer partial push for master by ref input
    -- FIXME print nvr
    when (tty && (not merged || (newrepo && length unmerged == 1))) $ prompt_ "Press Enter to push and build"
    gitPushSilent
  nvr <- pkgNameVerRel' br spec
  -- FIXME should compare git refs
  buildstatus <- kojiBuildStatus nvr
  case buildstatus of
    Just BuildComplete | isNothing scratch -> putStrLn $ nvr ++ " is already built"
    Just BuildBuilding | isNothing scratch -> putStrLn $ nvr ++ " is already building"
    _ -> do
      let tag = fromMaybe (branchDestTag br) mtarget
      mlatest <- kojiLatestNVR tag $ unPackage pkg
      if dropExtension nvr == dropExtension (fromMaybe "" mlatest)
        then error' $ nvr ++ " is already latest (modulo disttag)"
        else do
        krbTicket
        unpushed' <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
        clean <- gitBool "diff-index" ["--quiet", "HEAD"]
        let march = case scratch of
                      Just (Arch arch) -> ["--arch-override=" ++ arch]
                      _ -> []
            -- FIXME what if untracked files
            srpm = not (null unpushed') || not clean
        -- FIXME parse build output
        let target = fromMaybe (branchTarget br) mtarget
        if srpm
          then do
          srpmfile <- generateSrpm (Just br) spec
          void $ kojiBuild target $ march ++ ["--fail-fast", srpmfile]
          else kojiBuildBranch target (unPackage pkg) $ ["--fail-fast"] ++ ["--scratch" | isJust scratch] ++ march
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
          -- FIXME override option or autochain
          -- FIXME prompt for override note
          when (override && br /= Master) $ bodhiCreateOverride nvr
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
