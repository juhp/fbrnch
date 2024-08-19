module Cmd.CreateReview (
  createReviewCmd
  )
where

import Control.Monad.Extra (unless, unlessM)
import Data.Char (isAscii)
import SimpleCmd (cmd, error', (+-+))
import SimplePrompt (promptEnter)
import System.Directory (doesFileExist)

import Branches
import Bugzilla
import Package
import PkgReview
import RpmBuild

-- FIXME add --dependent pkgreview
-- FIXME verify tarball is same as upstream
-- FIXME post URL field too
createReviewCmd :: Maybe ScratchOption -> Bool -> [FilePath] -> IO ()
createReviewCmd mscratchOpt mock pkgs =
  withPackagesByBranches HeaderMust False Nothing Zero createPkgReview (Branches [], pkgs)
  where
    createPkgReview :: Package -> AnyBranch -> IO ()
    createPkgReview package _br = do
      let spec = packageSpec package
          pkg = unPackage package
      unlessM (doesFileExist spec) $
        error' $ "This does not look like a pkg dir:" +-+ spec +-+ "not found"
      unless (all isAscii pkg) $
        putStrLn "Warning: package name is not ASCII!"
      putStrLn "checking for existing reviews..."
      (bugs,session) <- bugsSession $ pkgReviews pkg
      unless (null bugs) $ do
        putStrLn "Existing review(s):"
        mapM_ putBug bugs
        -- FIXME abort if open review (unless --force?)
        promptEnter "Press Enter to continue"
      srpm <- generateSrpm Nothing spec
      mockRpmLint mock pkg spec srpm
      (mkojiurl,specSrpmUrls) <- buildAndUpload mscratchOpt srpm pkg spec
      bugid <- postReviewReq session spec specSrpmUrls mkojiurl pkg
      putStrLn "Review request posted:"
      putBugId bugid
      where
        postReviewReq :: BugzillaSession -> FilePath -> String -> Maybe String -> String -> IO BugId
        postReviewReq session spec specSrpmUrls mkojiurl pkg = do
          summary <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{summary}", spec]
          description <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{description}", spec]
          url <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{url}", spec]
          createBug session
            [ ("product", "Fedora")
            , ("component", "Package Review")
            , ("version", "rawhide")
            , ("summary", "Review Request: " <> pkg <> " - " <> summary)
            , ("url", url)
            , ("description", specSrpmUrls <> "\n\nDescription:\n" <> description <>  maybe "" ("\n\n\nKoji scratch build: " <>) mkojiurl)]
