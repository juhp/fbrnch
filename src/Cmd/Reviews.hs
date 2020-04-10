module Cmd.Reviews where

import Data.List
import Web.Bugzilla

import Bugzilla
import ListReviews

-- FIXME add --state or --new, --modified, etc
reviewsCmd :: ReviewStatus -> IO ()
reviewsCmd status =
  listReviews' True status >>= mapM_ putBug . sortOn (bugStatusEnum . bugStatus)
