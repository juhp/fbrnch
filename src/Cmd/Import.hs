{-# LANGUAGE OverloadedStrings #-}

module Cmd.Import (importCmd) where

import Common
import Common.System
import qualified Common.Text as T

import Network.URI

import Branches
import Bugzilla
import Cmd.RequestBranch (requestPkgBranches)
import Git
import Koji
import Krb
import ListReviews
import Package
import Prompt

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
-- FIXME add --dryrun
importCmd :: Bool -> (BranchesReq,[String]) -> IO ()
importCmd mock (breq, ps) = do
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
        -- FIXME check git repo exists
        clonePkg True UserClone Nothing pkg
        putStrLn ""
        setCurrentDirectory pkg
        -- FIXME: check branch is rawhide
      unlessM isGitRepo $ error' "Not a git repo"
      newrepo <- initialPkgRepo
      if not newrepo
        then putStrLn "Skipping: already imported"
        else do
        checkWorkingDirClean
        -- FIXME get session from importPkgs
        (bid,session) <- approvedReviewBugIdSession pkg
        putBugId bid
        putStrLn ""
        comments <- getComments session bid
        mapM_ showComment comments
        putStrLn ""
        putStr "Review bug: "
        putBugId bid
        putStrLn ""
        prompt_ "Press Enter to continue"
        let srpms = map (T.replace "/reviews//" "/reviews/") $ concatMap findSRPMs comments
        when (null srpms) $ error "No srpm urls found!"
        mapM_ T.putStrLn srpms
        let srpm = (head . filter isURI . filter (".src.rpm" `isSuffixOf`) . words . T.unpack . last) srpms
        let srpmfile = takeFileName srpm
        -- FIXME if havesrpm then print local filename
        prompt_ $ "Press Enter to import " ++ srpmfile
        havesrpm <- doesFileExist srpmfile
        unless havesrpm $
          cmd_ "curl" ["--silent", "--show-error", "--remote-name", srpm]
        krbTicket
        fedpkg_ "import" [srpmfile]
        git_ "commit" ["--message", "import #" ++ show bid]
        nvr <- pkgNameVerRel' Rawhide (pkg <.> "spec")
        prompt_ $ "Press Enter to push and build " ++ nvr
        gitPushSilent Nothing
        -- FIXME build more branches
        kojiBuildBranch "rawhide" (Package pkg) Nothing ["--fail-fast"]
        putBugBuild False session bid nvr
        existing <- fedoraBranchesNoRawhide (localBranches False)
        when (null existing) $ do
          brs <- getRequestedBranches breq
          requestPkgBranches False mock (Branches brs) (Package pkg)
      when (pkg /= takeFileName dir) $
        setCurrentDirectory dir
      where
        findSRPMs :: Comment -> [T.Text]
        findSRPMs =
          filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText
