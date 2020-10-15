module Cmd.Reviews (
  reviewsCmd,
  findReview)
where

import Bugzilla
import Common
import ListReviews

-- FIXME add --state or --new, --modified, etc
-- FIXME display time of last update
reviewsCmd :: Bool -> Bool -> Maybe String -> ReviewStatus -> IO ()
reviewsCmd short allstates muser status = do
  listReviews' allstates muser status >>=
    mapM_ (putReviewBug short) . sortBugsByStatus
  when short $ putStrLn ""

findReview :: String -> IO ()
findReview pkg = do
  (bugs, _) <- bugIdsSession $ pkgReviews pkg
  mapM_ putBugId bugs
