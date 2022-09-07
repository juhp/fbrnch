module Cmd.Reviews (
  reviewsCmd,
  findReview)
where

import Bugzilla
import Common
import ListReviews

-- FIXME add --state or --new, --modified, etc
-- FIXME display time of last update
reviewsCmd :: Bool -> Bool -> Bool -> Maybe String -> Maybe String
           -> ReviewStatus -> IO ()
reviewsCmd short allstates assignee muser mpat status = do
  listReviewsFull assignee muser mpat allstates status >>=
    mapM_ (putReviewBug short) . sortBugsByStatus . sortBugsByID
  when short $ putNewLn

findReview :: String -> IO ()
findReview pkg = do
  bugIdsAnon (pkgReviews pkg)
    >>= mapM_ putBugId
