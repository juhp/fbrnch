{-# LANGUAGE OverloadedStrings #-}

module ListReviews (
  ReviewStatus(..),
  listReviews,
  listReviewsAll,
  listReviewsFull
  ) where

import SimpleCmd (error')

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
listReviewsAll = listReviewsFull Nothing (Just Nothing) Nothing

-- FIXME: --unassigned
listReviewsFull :: Maybe (Maybe String) -> Maybe (Maybe String)
                -> Maybe String -> Bool -> ReviewStatus -> IO [Bug]
listReviewsFull Nothing Nothing Nothing _ _ =
  error' "please specify either report, assignee, or package name pattern"
listReviewsFull mmassignee mmreporter mpat allopen status = do
  session <- bzApiKeySession
  massignedto <-
    case mmassignee of
      Nothing -> return Nothing
      Just massignee ->
        Just <$> getBzAccountId session massignee
  mreporter <-
    case mmreporter of
      Nothing -> return Nothing
      Just mreporter ->
        Just <$> getBzAccountId session mreporter
  let reviews = maybe id (\a -> (assigneeIs a .&&.)) massignedto $
                maybe id (\r -> (reporterIs r .&&.)) mreporter $
                maybe packageReview pkgReviewsPrefix mpat
      open = if allopen
        then statusOpen else
        case status of
          ReviewAllOpen -> statusOpen
          ReviewUnApproved -> statusOpen
          ReviewApproved -> statusNewPost
          ReviewRepoCreated -> statusRelPrep
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
    notBranched pkg = null . delete Rawhide <$> listRemoteBranches pkg
