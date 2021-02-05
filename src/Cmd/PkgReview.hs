{-# LANGUAGE OverloadedStrings #-}

module Cmd.PkgReview (
  createReview,
  updateReview
  ) where

import Common
import Common.System
import qualified Common.Text as T

import Data.Char
import Network.HTTP.Directory

import Branches
import Bugzilla
import Koji
import Krb
import Package
import Prompt

-- FIXME add --dependent pkgreview
createReview :: Bool -> Bool -> [FilePath] -> IO ()
createReview noscratch mock pkgs =
  withPackageByBranches (Just True) Nothing Nothing True AnyNumber createPkgReview ("master":pkgs)
  where
    createPkgReview :: Package -> AnyBranch -> IO ()
    createPkgReview package _br = do
      let spec = packageSpec package
          pkg = unPackage package
      unless (all isAscii pkg) $
        putStrLn "Warning: package name is not ASCII!"
      putStrLn "checking for existing reviews..."
      (bugs,session) <- bugsSession $ pkgReviews pkg
      unless (null bugs) $ do
        putStrLn "Existing review(s):"
        mapM_ putBug bugs
        -- FIXME abort if open review (unless --force?)
        prompt_ "Press Enter to continue"
      srpm <- generateSrpm Nothing spec
      mockRpmLint mock pkg spec srpm
      (mkojiurl,specSrpmUrls) <- buildAndUpload noscratch srpm pkg spec
      bugid <- postReviewReq session spec specSrpmUrls mkojiurl pkg
      putStrLn "Review request posted:"
      putBugId bugid
      where
        postReviewReq :: BugzillaSession -> FilePath -> String -> Maybe String -> String -> IO BugId
        postReviewReq session spec specSrpmUrls mkojiurl pkg = do
          summary <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
          description <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
          createBug session
            [ ("product", "Fedora")
            , ("component", "Package Review")
            , ("version", "rawhide")
            , ("summary", "Review Request: " <> pkg <> " - " <> summary)
            , ("description", specSrpmUrls <> "\n\nDescription:\n" <> description <>  maybe "" ("\n\n\nKoji scratch build: " <>) mkojiurl)]

buildAndUpload :: Bool -> String -> String -> FilePath
               -> IO (Maybe String, String)
buildAndUpload noscratch srpm pkg spec = do
  mkojiurl <- if noscratch then return Nothing
    else Just <$> kojiScratchBuild "rawhide" [] srpm
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  return (mkojiurl, specSrpmUrls)

updateReview :: Bool -> Bool -> Maybe FilePath -> IO ()
updateReview noscratch mock mspec = do
  spec <- maybe findSpecfile checkLocalFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  (bid,session) <- reviewBugIdSession pkg
  putBugId bid
  srpm <- generateSrpm Nothing spec
  submitted <- checkForComment session bid (T.pack srpm)
  when submitted $
    error' "This NVR was already posted on the review bug: please bump"
  mockRpmLint mock pkg spec srpm
  (mkojiurl,specSrpmUrls) <- buildAndUpload noscratch srpm pkg spec
  changelog <- getChangeLog spec
  commentBug session bid (specSrpmUrls <> (if null changelog then "" else "\n\n" <> changelog) <> maybe "" ("\n\nKoji scratch build: " <>) mkojiurl)
  -- putStrLn "Review bug updated"
  where
    checkLocalFile :: FilePath -> IO FilePath
    checkLocalFile f =
      if takeFileName f == f then return f
        else error' "Please run in the directory of the spec file"

uploadPkgFiles :: String -> FilePath -> FilePath -> IO String
uploadPkgFiles pkg spec srpm = do
  fasid <- fasIdFromKrb
  -- read ~/.config/fedora-create-review
  let sshhost = "fedorapeople.org"
      sshpath = "public_html/reviews/" ++ pkg
  cmd_ "ssh" [fasid ++ "@" ++ sshhost, "mkdir", "-p", sshpath]
  cmd_ "scp" [spec, srpm, sshhost ++ ":" ++ sshpath]
  getCheckedFileUrls $ "https://" <> fasid ++ "@" ++ fasid <> ".fedorapeople.org" </> removePrefix "public_html/" sshpath
  where
    getCheckedFileUrls :: String -> IO String
    getCheckedFileUrls uploadurl = do
      let specUrl = uploadurl </> takeFileName spec
          srpmUrl = uploadurl </> takeFileName srpm
      mgr <- httpManager
      checkUrlOk mgr specUrl
      checkUrlOk mgr srpmUrl
      return $ "Spec URL: " <> specUrl <> "\nSRPM URL: " <> srpmUrl
      where
        checkUrlOk mgr url = do
          okay <- httpExists mgr url
          unless okay $ error' $ "Could not access: " ++ url

mockRpmLint :: Bool -> String -> FilePath -> FilePath -> IO ()
mockRpmLint mock pkg spec srpm = do
  rpms <-
    if mock then do
      -- FIXME check that mock is installed
      let resultsdir = "results_" ++ pkg
      cmd_ "mock" ["--root", mockConfig Rawhide, "--resultdir=" ++ resultsdir, srpm]
      map (resultsdir </>) . filter ((== ".rpm") . takeExtension) <$> listDirectory resultsdir
    else
      builtRpms (RelBranch Rawhide) spec >>= filterM doesFileExist
  -- FIXME run rpmlint on spec too??
  void $ cmdBool "rpmlint" $ srpm:rpms
  prompt_ "Press Enter to submit"
