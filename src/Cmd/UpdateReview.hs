module Cmd.UpdateReview (
    updateReviewCmd
    )
where

import Control.Monad (when)
import SimpleCmd (cmd, error')
import System.FilePath (takeFileName)

import Bugzilla
--import Common
import qualified Common.Text as T
import Package
import PkgReview
import RpmBuild
import Types (ChangeType(ChangeReview))

updateReviewCmd :: Maybe ScratchOption -> Bool -> Maybe FilePath -> IO ()
updateReviewCmd mscratchOpt mock mspec = do
  spec <- maybe findSpecfile checkLocalFile mspec
  pkg <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
  (bid,session) <- reviewBugIdSession pkg
  putBugId bid
  srpm <- generateSrpm Nothing spec
  submitted <- checkForComment session bid (T.pack srpm)
  when submitted $
    error' "This NVR was already posted on the review bug: please bump"
  mockRpmLint mock pkg spec srpm
  (mkojiurl,specSrpmUrls) <- buildAndUpload mscratchOpt srpm pkg spec
  changelog <- changeLogPrompt ChangeReview spec
  commentBug session bid (specSrpmUrls <> (if null changelog then "" else "\n\n" <> changelog) <> maybe "" ("\n\nKoji scratch build: " <>) mkojiurl)
  putBugId bid
  where
    checkLocalFile :: FilePath -> IO FilePath
    checkLocalFile f =
      if takeFileName f == f then return f
        else error' "Please run in the directory of the spec file"
