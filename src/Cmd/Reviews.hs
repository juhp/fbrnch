module Cmd.Reviews (
  reviewsCmd,
  findReview)
where

import Bugzilla
import Common
import ListReviews

-- FIXME add --state or --new, --modified, etc
-- FIXME display time of last update
reviewsCmd :: Bool -> Bool -> Maybe (Maybe String) -> Maybe (Maybe String)
           -> Maybe String -> ReviewStatus -> IO ()
reviewsCmd short allstates Nothing Nothing Nothing status =
  reviewsCmd short allstates Nothing (Just Nothing) Nothing status
reviewsCmd short allstates mmassignee mmreporter mpat status = do
  listReviewsFull mmassignee mmreporter mpat allstates status >>=
    mapM_ (putReviewBug short) . sortBugsByStatus . sortBugsByID
  when short putNewLn

findReview :: String -> IO ()
findReview pkg = do
  bugIdsAnon (pkgReviews pkg)
    >>= mapM_ putBugId
