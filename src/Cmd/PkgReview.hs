module Cmd.PkgReview (
  createReview,
  updateReview
  ) where

import Common
import Common.System
import qualified Common.Text as T

import Data.Char
import Network.HTTP.Directory
import Network.HTTP.Simple

import Bugzilla
import Bugzilla.NewId
import Koji
import Krb
import Package
import Prompt

-- FIXME add --mock option
-- FIXME add --dependent pkgreview
createReview :: Bool -> Bool -> Maybe FilePath -> IO ()
createReview noscratch mock mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  unless (spec == pkg <.> "spec") $
    putStrLn "Warning: package name and spec filename differ!"
  unless (all isAscii pkg) $
    putStrLn "Warning: package name is not ASCII!"
  putStrLn "checking for existing reviews..."
  (bugs,session) <- bugsSession $ pkgReviews pkg
  unless (null bugs) $ do
    putStrLn "Existing review(s):"
    mapM_ putBug bugs
    prompt_ "Press Enter to continue"
  srpm <- generateSrpm Nothing spec
  mockRpmLint mock pkg spec srpm
  mkojiurl <- kojiScratchUrl noscratch srpm
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  bugid <- postReviewReq session spec specSrpmUrls mkojiurl pkg
  putStrLn "Review request posted:"
  putBugId bugid
  where
    postReviewReq :: BugzillaSession -> FilePath -> String -> Maybe String -> String -> IO BugId
    postReviewReq session spec specSrpmUrls mkojiurl pkg = do
      summary <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
      description <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
      let req = setRequestMethod "POST" $
              setRequestCheckStatus $
              newBzRequest session ["bug"]
              [ makeTextItem "product" "Fedora"
              , makeTextItem "component" "Package Review"
              , makeTextItem "version" "rawhide"
              , makeTextItem "summary" $ "Review Request: " <> pkg <> " - " <> summary
              , makeTextItem "description" $ specSrpmUrls <> "\n\nDescription:\n" <> description <>  maybe "" ("\n\n\nKoji scratch build: " <>) mkojiurl
              ]
      newId . getResponseBody <$> httpJSON req

getSpecFile :: Maybe FilePath -> IO String
getSpecFile =
  -- FIXME or change to dir
  maybe findSpecfile checkLocalFile
  where
    checkLocalFile :: FilePath -> IO FilePath
    checkLocalFile f =
      if takeFileName f == f then return f
        else error' "Please run in the directory of the spec file"

updateReview :: Bool -> Bool -> Maybe FilePath -> IO ()
updateReview noscratch mock mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  (bid,session) <- reviewBugIdSession pkg
  putBugId bid
  srpm <- generateSrpm Nothing spec
  submitted <- checkForComment session bid (T.pack srpm)
  when submitted $
    error' "This NVR was already posted on the review bug: please bump"
  mockRpmLint mock pkg spec srpm
  mkojiurl <- kojiScratchUrl noscratch srpm
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  changelog <- getChangeLog spec
  postComment session bid (specSrpmUrls <> (if null changelog then "" else "\n\n" <> changelog) <> maybe "" ("\n\nKoji scratch build: " <>) mkojiurl)
  -- putStrLn "Review bug updated"

uploadPkgFiles :: String -> FilePath -> FilePath -> IO String
uploadPkgFiles pkg spec srpm = do
  fasid <- fasIdFromKrb
  -- read ~/.config/fedora-create-review
  let sshhost = "fedorapeople.org"
      sshpath = "public_html/reviews/" ++ pkg
  cmd_ "ssh" [sshhost, "mkdir", "-p", sshpath]
  cmd_ "scp" [spec, srpm, sshhost ++ ":" ++ sshpath]
  getCheckedFileUrls $ "https://" <> fasid <> ".fedorapeople.org" </> removePrefix "public_html/" sshpath
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
  rpms <- if mock then do
    -- FIXME check that mock is installed
    let resultsdir = "results_" ++ pkg
    cmd_ "mock" ["--resultdir=" ++ resultsdir, srpm]
    map (resultsdir </>) . filter ((== ".rpm") . takeExtension) <$> listDirectory resultsdir
    else do
    br <- systemBranch
    builtRpms br spec >>= filterM doesFileExist
  -- FIXME run rpmlint on spec too??
  void $ cmdBool "rpmlint" $ srpm:rpms
  prompt_ "Press Enter to submit"
