module Cmd.Build (
  buildCmd,
  Scratch(..)
  ) where

import Common
import Common.System

import System.IO (hIsTerminalDevice, stdin)
import Web.Fedora.Bodhi hiding (bodhiUpdate)

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
buildCmd :: Bool -> Maybe Scratch -> Maybe String -> ([Branch],[Package]) -> IO ()
buildCmd merge scratch mtarget (brs,pkgs) = do
  when (isJust mtarget && length brs > 1) $
    error' "You can only specify target with one branch"
  withPackageBranches True (buildBranch False merge scratch mtarget) (brs,pkgs)

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
  let spec = pkg <.> "spec"
  -- FIXME offer merge if newer branch has commits
  when (not (null unpushed) && isNothing scratch) $ do
    checkSourcesMatch spec
    tty <- hIsTerminalDevice stdin
    when tty $ prompt_ "Press Enter to push and build"
    gitPushSilent
  checkForSpecFile spec
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
          changelog <- getChangeLog spec
          bodhiUpdate mbid changelog nvr
          -- override option or autochain
          -- FIXME need prompt for override note
          --when False $ bodhiOverrides "save" "FIXME" nvr
  where
    bodhiUpdate :: Maybe BugId -> String -> String -> IO ()
    bodhiUpdate mbid changelog nvr = do
      let bugs = maybe [] (\b -> ["--bugs", show b]) mbid
      -- FIXME check for autocreated update (pre-updates-testing)
      -- also query for open bugs
      putStrLn $ "Creating Bodhi Update for " ++ nvr ++ ":"
      updateOK <- cmdBool "bodhi" (["updates", "new", "--type", if isJust mbid then "newpackage" else "enhancement", "--notes", changelog, "--autokarma", "--autotime", "--close-bugs"] ++ bugs ++ [nvr])
      unless updateOK $ do
        updatequery <- bodhiUpdates [makeItem "display_user" "0", makeItem "builds" nvr]
        case updatequery of
          [] -> do
            putStrLn "bodhi submission failed"
            prompt_ "Press Enter to resubmit to Bodhi"
            bodhiUpdate mbid changelog nvr
          [update] -> case lookupKey "url" update of
            Nothing -> error' "Update created but no url"
            Just uri -> putStrLn uri
          _ -> error' $ "impossible happened: more than one update found for " ++ nvr

    checkSourcesMatch :: FilePath -> IO ()
    checkSourcesMatch spec = do
      -- "^[Ss]ource[0-9]*:"
      source <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
      sources <- lines <$> readFile "sources"
      when (length source /= length sources) $
        putStrLn "Different numbers of Source fields and sources lines"
      forM_ source $ \ src ->
        when (isNothing (find (src `isInfixOf`) sources)) $
        -- FIXME offer to add source
        error' $ src ++ " not in sources"
