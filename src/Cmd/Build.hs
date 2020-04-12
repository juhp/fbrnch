module Cmd.Build (buildCmd, buildBranches) where

import Common
import Common.System

import Distribution.Fedora.Branch
import System.IO (hIsTerminalDevice, stdin)
import Web.Bugzilla

import Bugzilla
import Branches
import Cmd.Merge
import Git
import Krb
import Koji
import Package
import Prompt
import Types

-- FIXME sort packages in build dependency order (chain-build?)
-- FIXME --no-fast-fail
buildCmd :: Bool -> Maybe Scratch -> Maybe String -> [Branch] -> [Package] -> IO ()
buildCmd merge scratch mtarget brs pkgs =
  if null pkgs
  then do
    branches <- if null brs then packageBranches else return brs
    mapM_ (buildBranch False merge scratch mtarget Nothing) branches
  else mapM_ buildPkg pkgs
  where
    buildPkg :: Package -> IO ()
    buildPkg pkg =
      withExistingDirectory pkg $ do
        checkWorkingDirClean
        git_ "fetch" []
        branches <- if null brs then packageBranches else return brs
        when (null brs) $
          putStrLn $ "\nBranches: " ++ unwords (map show branches)
        buildBranches False merge scratch mtarget (Just pkg) branches

buildBranches :: Bool -> Bool -> Maybe Scratch -> Maybe String -> Maybe Package -> [Branch] -> IO ()
buildBranches _ _ _ _ _ [] = return ()
buildBranches pulled merge scratch mtarget mpkg brs = do
  when (isJust mtarget && length brs > 1) $
    error' "You can only specify target with one branch"
  mapM_ (buildBranch pulled merge scratch mtarget mpkg) brs

buildBranch :: Bool -> Bool -> Maybe Scratch -> Maybe String -> Maybe Package -> Branch -> IO ()
buildBranch pulled merge scratch mtarget mpkg br = do
  clean <- workingDirClean
  when (not clean && isNothing scratch) $
    error' "Working dir is not clean"
  unless pulled
    gitPull
  pkg <- getPackageName mpkg
  putPkgBrnchHdr pkg br
  branched <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
  if not branched
    then error' $ show br ++ " branch does not exist!"
    else switchBranch br
  newrepo <- initialPkgRepo
  when (merge || newrepo) $
    mergeBranch True True (Just pkg) br
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  when (not merge || br == Master) $
    -- FIXME hide if just merged
    unless (null unpushed) $ do
      putStrLn "Local commits:"
      mapM_ (putStrLn . simplifyCommitLog) unpushed
  -- FIXME offer merge if newer branch has commits
  when (not (null unpushed) && isNothing scratch) $ do
    tty <- hIsTerminalDevice stdin
    when tty $ prompt_ "to push and build"
    gitPushSilent
  checkForSpecFile pkg
  nvr <- fedpkg "verrel" []
  -- FIXME should compare git refs
  buildstatus <- kojiBuildStatus nvr
  case buildstatus of
    COMPLETE | isNothing scratch -> putStrLn $ nvr ++ " is already built"
    BUILDING | isNothing scratch -> putStrLn $ nvr ++ " is already building"
    _ -> do
      let tag = fromMaybe (branchDestTag br) mtarget
      latest <- cmd "koji" ["latest-build", "--quiet", tag , pkg]
      if dropExtension nvr == dropExtension latest
        then error' $ nvr ++ " is already latest (modulo disttag)"
        else do
        krbTicket
        unpushed' <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
        let march = case scratch of
              Just (Arch arch) -> Just arch
              _ -> Nothing
            srpm = not (null unpushed') || not clean
        -- FIXME use koji directly
        -- FIXME parse build output
        fedpkg_ "build" $ ["--fail-fast"] ++ ["--scratch" | isJust scratch] ++ (if isJust march then ["--arch", fromJust march] else []) ++ ["--srpm" | srpm] ++ (if isJust mtarget then ["--target", tag] else [])
        --waitForbuild
        -- FIXME check if first build, also add --bz and short buglists query
        (mbid,session) <- bzReviewSession
        if br == Master
          then forM_ mbid $ postBuildComment session nvr
          else do
          -- FIXME diff previous changelog?
          changelog <- getChangeLog $ pkg <.> "spec"
          bodhiUpdate mbid changelog nvr
          -- override option or autochain
          when False $ cmd_ "bodhi" ["overrides", "save", nvr]
  where
    bodhiUpdate :: Maybe BugId -> String -> String -> IO ()
    bodhiUpdate mbid changelog nvr = do
      let bugs = maybe [] (\b -> ["--bugs", show b]) mbid
      -- FIXME check for autocreated update (pre-updates-testing)
      -- also query for open bugs
      putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
      updateOK <- cmdBool "bodhi" (["updates", "new", "--type", if isJust mbid then "newpackage" else "enhancement", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
      unless updateOK $ do
        updatequery <- cmdLines "bodhi" ["updates", "query", "--builds", nvr]
        if last updatequery == "1 updates found (1 shown)"
          then putStrLn $ (unlines . init) updatequery
          else do
          putStrLn "bodhi submission failed"
          prompt_ "to resubmit to Bodhi"
          bodhiUpdate mbid changelog nvr
