module Cmd.Reviews (
  reviewsCmd,
  findReview)
where

import Bugzilla
import ListReviews

-- FIXME add --state or --new, --modified, etc
-- FIXME display time of last update
reviewsCmd :: Bool -> Bool -> ReviewStatus -> IO ()
reviewsCmd short allstates status =
  listReviews' allstates status >>=
  mapM_ (putReviewBug short) . sortBugsByStatus

findReview :: String -> IO ()
findReview pkg = do
  (bugs, _) <- bugIdsSession $ pkgReviews pkg
  mapM_ putBugId bugs
