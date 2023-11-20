{-# LANGUAGE OverloadedStrings #-}

module ListReviews (
  ReviewStatus(..),
  listReviews,
  listReviewsAll,
  listReviewsFull
  ) where

import Common

import Branches
import Bugzilla

data ReviewStatus = ReviewAllOpen
                  | ReviewUnApproved
                  | ReviewApproved
                  | ReviewWithoutRepoReq
                  | ReviewRepoRequested
                  | ReviewRepoCreated
                  | ReviewUnbranched
                  | ReviewBranched

listReviews :: ReviewStatus -> IO [Bug]
listReviews = listReviewsAll False

listReviewsAll :: Bool -> ReviewStatus -> IO [Bug]
listReviewsAll = listReviewsFull False Nothing Nothing

-- FIXME: --unassigned
listReviewsFull :: Bool -> Maybe String -> Maybe String -> Bool
                -> ReviewStatus-> IO [Bug]
listReviewsFull assignee muser mpat allopen status = do
  session <- bzApiKeySession
  accountid <- getBzAccountId session muser
  let reviews = (if assignee then assigneeIs else reporterIs) accountid .&&. maybe packageReview pkgReviewsPrefix mpat
      open = if allopen
        then statusOpen else
        case status of
          ReviewAllOpen -> statusOpen
          ReviewUnApproved -> statusOpen
          ReviewApproved -> statusNewPost
          ReviewRepoCreated -> statusNewPost
          ReviewUnbranched -> statusNewModified
          ReviewBranched -> statusNewModified
          _ -> statusNewPost
      query = case status of
        ReviewAllOpen -> reviews
        ReviewUnApproved -> reviews .&&. not' reviewApproved
        _ -> reviews .&&. reviewApproved
  -- FIXME sort by status, bid (default?) / pkg?
  bugs <- searchBugs session (query .&&. open)
  case status of
    ReviewWithoutRepoReq ->
      filterM (fmap not . checkRepoRequestedComment session . bugId) bugs
    ReviewRepoRequested ->
      filterM (checkRepoRequestedComment session . bugId) bugs >>=
      filterM (fmap not . checkRepoCreatedComment session . bugId)
    ReviewRepoCreated ->
      filterM (checkRepoCreatedComment session . bugId) bugs
    ReviewUnbranched ->
      filterM (checkRepoCreatedComment session . bugId) bugs >>=
      filterM (notBranched . reviewBugToPackage)
    ReviewBranched ->
      filterM (checkRepoCreatedComment session . bugId) bugs >>=
      filterM (branched . reviewBugToPackage)
    _ -> return bugs
  where
    checkRepoRequestedComment :: BugzillaSession -> BugId -> IO Bool
    checkRepoRequestedComment session bid =
        checkForComment session bid
          "https://pagure.io/releng/fedora-scm-requests/issue/"

    branched :: String -> IO Bool
    branched pkg = not <$> notBranched pkg

    notBranched :: String -> IO Bool
    notBranched pkg = null <$> fedoraBranchesNoRawhide (pagurePkgBranches pkg)
