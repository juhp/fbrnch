{-# LANGUAGE CPP #-}

module Cmd.PkgReview where

import Control.Monad
import Data.Char
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmd
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process.Text (readProcessWithExitCode)
import Web.Bugzilla (BugId, BugzillaSession, newBzRequest)

import Bugzilla
import Bugzilla.NewId
import Koji
import Krb
import Package
import Prompt

createReview :: Bool -> Maybe FilePath -> IO ()
createReview noscratch mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  unless (all isAscii pkg) $
    putStrLn "Warning: package name is not ASCII!"
  (bugs,session) <- bugsSession $ pkgReviews pkg
  unless (null bugs) $ do
    putStrLn "Existing review(s):"
    mapM_ putBug bugs
    prompt_ "to continue"
  srpm <- generateSrpm spec
  mkojiurl <- kojiScratchUrl noscratch srpm
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  bugid <- postReviewReq session spec specSrpmUrls mkojiurl pkg
  putStrLn "Review request posted:"
  putBugId bugid
  where
    postReviewReq :: BugzillaSession -> FilePath -> T.Text -> Maybe String -> String -> IO BugId
    postReviewReq session spec specSrpmUrls mkojiurl pkg = do
      summary <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
      description <- cmdT "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
      let req = setRequestMethod "POST" $
              setRequestCheckStatus $
              newBzRequest session ["bug"]
              [ ("product", Just "Fedora")
              , ("component", Just "Package Review")
              , ("version", Just "rawhide")
              , ("summary", Just $ "Review Request: " <> T.pack pkg <> " - " <> summary)
              , ("description", Just $ specSrpmUrls <> "\n\nDescription:\n" <> description <>  maybe "" ("\n\n\nKoji scratch build: " <>) (T.pack <$> mkojiurl))
              ]
      newId . getResponseBody <$> httpJSON req

updateReview :: Bool -> Maybe FilePath -> IO ()
updateReview noscratch mspec = do
  spec <- getSpecFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  (bid,session) <- reviewBugIdSession pkg
  putBugId bid
  srpm <- generateSrpm spec
  submitted <- checkForComment session bid (T.pack srpm)
  when submitted $
    error' "This NVR was already posted on the review bug: please bump"
  mkojiurl <- kojiScratchUrl noscratch srpm
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  changelog <- getChangeLog spec
  postComment session bid (specSrpmUrls <> (if null changelog then "" else "\n\n" <> T.pack changelog) <> maybe "" ("\n\nKoji scratch build: " <>) (T.pack <$> mkojiurl))
  -- putStrLn "Review bug updated"

uploadPkgFiles :: String -> FilePath -> FilePath -> IO T.Text
uploadPkgFiles pkg spec srpm = do
  fasid <- fasIdFromKrb
  -- read ~/.config/fedora-create-review
  let sshhost = "fedorapeople.org"
      sshpath = "public_html/reviews/" ++ pkg
  cmd_ "ssh" [sshhost, "mkdir", "-p", sshpath]
  cmd_ "scp" [spec, srpm, sshhost ++ ":" ++ sshpath]
  getCheckedFileUrls $ "https://" <> fasid <> ".fedorapeople.org" </> removePrefix "public_html/" sshpath
  where
    getCheckedFileUrls :: String -> IO T.Text
    getCheckedFileUrls uploadurl = do
      let specUrl = uploadurl </> takeFileName spec
          srpmUrl = uploadurl </> takeFileName srpm
      mgr <- httpManager
      checkUrlOk mgr specUrl
      checkUrlOk mgr srpmUrl
      return $ "Spec URL: " <> T.pack specUrl <> "\nSRPM URL: " <> T.pack srpmUrl
      where
        checkUrlOk mgr url = do
          okay <- httpExists mgr url
          unless okay $ error' $ "Could not access: " ++ url

cmdT :: String -> [String] -> IO T.Text
cmdT c args = do
  (ret, out, err) <- readProcessWithExitCode c args ""
  case ret of
    ExitSuccess -> return out
    ExitFailure n -> error' $ unwords (c:args) +-+ "failed with status" +-+ show n ++ "\n" ++ T.unpack err
