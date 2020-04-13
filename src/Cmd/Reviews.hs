module Cmd.Reviews (reviewsCmd, review) where

import Bugzilla
import ListReviews

-- FIXME add --state or --new, --modified, etc
reviewsCmd :: ReviewStatus -> IO ()
reviewsCmd status =
  listReviews' True status >>= mapM_ putBug . sortBugsByStatus

review :: String -> IO ()
review pkg = do
  (bugs, _) <- bugIdsSession $ pkgReviews pkg
  mapM_ putBugId bugs
