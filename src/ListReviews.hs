{-# LANGUAGE OverloadedStrings #-}

module ListReviews (
  ReviewStatus(..),
  listReviews,
  listReviewsAll,
  listReviewsFull
  ) where

import Common
import Common.System
import qualified Common.Text as T
import Network.HTTP.Query

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

-- FIXME should not require login
listReviewsFull :: Bool -> Maybe String -> Maybe String -> Bool
                -> ReviewStatus-> IO [Bug]
listReviewsFull assignee muser mpat allopen status = do
  (session,user) <- bzLoginSession
  accountid <- do
    case muser of
      Nothing -> return user
      Just userid ->
        if emailIsValid userid then return $ T.pack userid
        else do
          users <- listBzUsers session userid
          case users of
            [] -> error' $ "No user found for " ++ userid
            [obj] -> return $ T.pack $ lookupKey' "email" obj
            objs -> error' $ "Found multiple user matches: " ++
                    unwords (map (lookupKey' "email") objs)
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
