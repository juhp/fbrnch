{-# LANGUAGE OverloadedStrings #-}

module Cmd.Import (
  importCmd,
  downloadReviewSRPM,
  upstreamDir
  )
where

import Common
import Common.System
import qualified Common.Text as T

import Fedora.Krb (krbTicket)
import Network.URI
import SimplePrompt (promptEnter)
import SimplePrompt.Internal (runPrompt, mapInput, getPromptLine)

import Branches
import Bugzilla
import Cmd.RequestBranch (requestPkgBranches, waitForKojiPkgBranch)
import Git
import Koji
import ListReviews
import Package

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
-- FIXME add --dryrun
importCmd :: Bool -> Bool -> Bool -> Bool -> (BranchesReq,[String]) -> IO ()
importCmd showbug reporequest existingrepo mock (breq, ps) = do
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
          clonePkg False True UserClone Nothing pkg
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
        srpmfile <- downloadReviewSRPM showbug False True pkg bid session
        putNewLn
        promptEnter $ "Press Enter to import" +-+ srpmfile
        krbTicket
        fedpkg_ "import" [srpmfile]
        git_ "commit" ["--message", "Import rhbz#" ++ show bid]
        nvr <- pkgNameVerRel' Rawhide (pkg <.> "spec")
        mbuild <- promptPushBuild $ "Press Enter to push & [B]uild" +-+ showNVR nvr ++ ", or just [P]ush, or [N]o to skip? [Y/n/b/p]"
        when (mbuild /= Just False) $ do
          gitPush True Nothing
          when (mbuild == Just True) $ do
            -- FIXME check package exists in koji (for instant import)
            -- FIXME build more branches
            kojiBuildBranch "rawhide" (Package pkg) Nothing ["--fail-fast"]
            putBugBuild False session bid nvr
        existing <- fedoraBranchesNoRawhide (localBranches False)
        when (null existing) $ do
          brs <- getRequestedBranches [] breq
          unless (reporequest || null brs) $
            requestPkgBranches False False mock (Branches brs) (Package pkg) >>=
            mapM_ (waitForKojiPkgBranch False (Package pkg))
      when (pkg /= takeFileName dir) $
        setCurrentDirectory dir

    promptPushBuild = runPrompt . mapInput maybePush . getPromptLine
      where
        maybePush inp =
          case lower inp of
            "" -> Just (Just True)
            "y" -> Just (Just True)
            "yes" -> Just (Just True)
            "b" -> Just (Just True)
            "build" -> Just (Just True)
            "p" -> Just Nothing
            "push" -> Just Nothing
            "n" -> Just (Just False)
            "no" -> Just (Just False)
            _ -> Nothing

downloadReviewSRPM :: Bool -> Bool -> Bool -> String -> Int -> BugzillaSession
                   -> IO FilePath
downloadReviewSRPM showbug getspec prompt pkg bid session = do
  comments <- getComments session bid
  when showbug $ do
    putNewLn
    mapM_ showComment comments
  putNewLn
  putStr "Review bug: "
  putBugId bid
  putNewLn
  when prompt $
    promptEnter "Press Enter to continue"
  when getspec $
    downloadSpec comments
  let srpms = map (T.replace "/reviews//" "/reviews/") $ concatMap findSRPMs comments
  mapM_ T.putStrLn srpms
  unless (null srpms) putNewLn
  downloadSRPM (last srpms)
  where
    findSRPMs :: Comment -> [T.Text]
    findSRPMs =
      filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText

    findSpecs :: Comment -> [T.Text]
    findSpecs =
      filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["spec url:", "spec:", "new spec:", "updated spec:"] && T.pack (pkg <.> "spec") `T.isSuffixOf` l) . T.lines . commentText

    maybeEncodeURI cs =
      if '%' `elem` cs
      then cs
      else escapeURIString isUnescapedInURI cs

    downloadSRPM :: T.Text -> IO FilePath
    downloadSRPM srpm = do
      case (filter (".src.rpm" `isSuffixOf`) . words . T.unpack) srpm of
        [] -> error' "no srpm filename found"
        srcrpms ->
          case filter (isURI . maybeEncodeURI) srcrpms of
            [] -> error "no valid srpm urls found"
            [srpmurl] -> do
              let srpmfile = takeFileName srpmurl
              -- FIXME if havesrpm then print local filename
              havesrpm <- doesFileExist srpmfile
              if havesrpm
                then putStrLn $ srpmfile +-+ "already exists"
                else
                -- was "--silent"
                cmd_ "curl" ["--show-error", "--remote-name", srpmurl]
              return srpmfile
            srpmurls -> error' $ "multiple srpm urls:" +-+ unwords srpmurls

    downloadSpec :: [Comment] -> IO ()
    downloadSpec comments = do
      let specs = map (T.replace "/reviews//" "/reviews/") $ concatMap findSpecs comments
      mapM_ T.putStrLn specs
      unless (null specs) putNewLn
      case (filter ((pkg <.> "spec") `isSuffixOf`) . words . T.unpack . last) specs of
        [] -> error' "no spec filename found"
        specfiles ->
          case filter (isURI . maybeEncodeURI) specfiles of
            [] -> error "no valid spec urls found"
            [specurl] -> do
              let specfile = upstreamDir </> takeFileName specurl
              havespec <- doesFileExist specfile
              if havespec
                then putStrLn $ specfile +-+ "already exists"
                else
                cmd_ "curl" ["--silent", "--show-error", "--create-dirs", "--output", specfile, specurl]
            specurls -> error' $ "multiple spec urls:" +-+ unwords specurls

upstreamDir :: FilePath
upstreamDir = "UPSTREAM"
