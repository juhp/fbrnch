{-# LANGUAGE OverloadedStrings #-}

module Cmd.Import (importCmd) where

import Common
import Common.System
import qualified Common.Text as T

import Network.URI
import SimplePrompt (promptEnter, yesNoDefault)

import Branches
import Bugzilla
import Cmd.RequestBranch (requestPkgBranches)
import Git
import Koji
import Krb
import ListReviews
import Package

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
-- FIXME add --dryrun
importCmd :: Bool -> Bool -> (BranchesReq,[String]) -> IO ()
importCmd existingrepo mock (breq, ps) = do
  pkgs <- if null ps
    then map reviewBugToPackage <$> listReviews ReviewRepoCreated
    else return ps
  mapM_ importPkg pkgs
  where
    -- FIXME check not in a different git dir
    importPkg :: String -> IO ()
    importPkg pkg = do
      putPkgHdr (Package pkg)
      dir <- getCurrentDirectory
      when (pkg /= takeFileName dir) $ do
        exists <- doesDirectoryExist pkg
        unless exists $ do
          clonePkg True UserClone Nothing pkg
          putNewLn
        setCurrentDirectory pkg
        -- FIXME: check branch is rawhide
      unlessM isGitRepo $ error' "Not a git repo"
      newrepo <- initialPkgRepo
      if not newrepo && not existingrepo
        then putStrLn "Skipping: already imported"
        else do
        checkWorkingDirClean False
        -- FIXME get session from importPkgs
        (bid,session) <- approvedReviewBugIdSession pkg
        putBugId bid
        putNewLn
        comments <- getComments session bid
        mapM_ showComment comments
        putNewLn
        putStr "Review bug: "
        putBugId bid
        putNewLn
        promptEnter "Press Enter to continue"
        let srpms = map (T.replace "/reviews//" "/reviews/") $ concatMap findSRPMs comments
        mapM_ T.putStrLn srpms
        case (filter (".src.rpm" `isSuffixOf`) . words . T.unpack . last) srpms of
          [] -> error' "no srpm filename found"
          srcrpms ->
            case filter (isURI . maybeEncodeURI) srcrpms of
              [] -> error "no valid srpm urls found"
              [srpmurl] -> do
                let srpmfile = takeFileName srpmurl
                -- FIXME if havesrpm then print local filename
                promptEnter $ "Press Enter to import" +-+ srpmfile
                havesrpm <- doesFileExist srpmfile
                unless havesrpm $
                  cmd_ "curl" ["--silent", "--show-error", "--remote-name", srpmurl]
                krbTicket
                fedpkg_ "import" [srpmfile]
                git_ "commit" ["--message", "import rhbz#" ++ show bid]
                nvr <- pkgNameVerRel' Rawhide (pkg <.> "spec")
                ok <- yesNoDefault True $ "Press Enter to push and build" +-+ showNVR nvr
                when ok $ do
                  gitPush True Nothing
                  -- FIXME build more branches
                  kojiBuildBranch "rawhide" (Package pkg) Nothing ["--fail-fast"]
                  putBugBuild False session bid nvr
                existing <- fedoraBranchesNoRawhide (localBranches False)
                when (null existing) $ do
                  brs <- getRequestedBranches [] breq
                  requestPkgBranches False False mock (Branches brs) (Package pkg)
              srpmurls -> error' $ "multiple srpm urls:" +-+ unwords srpmurls
      when (pkg /= takeFileName dir) $
        setCurrentDirectory dir

      where
        findSRPMs :: Comment -> [T.Text]
        findSRPMs =
          filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText

        maybeEncodeURI cs =
          if '%' `elem` cs
          then cs
          else escapeURIString isUnescapedInURI cs
