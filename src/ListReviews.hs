module ListReviews where

import Control.Monad
import Web.Bugzilla
import Web.Bugzilla.Search

import Branches
import Bugzilla

data ReviewStatus = ReviewAllOpen
                  | ReviewUnApproved
                  | ReviewApproved
                  | ReviewWithoutRepoReq
                  | ReviewRepoRequested
                  | ReviewRepoCreated
                  | ReviewUnbranched

listReviews :: ReviewStatus -> IO [Bug]
listReviews = listReviews' False

listReviews' :: Bool -> ReviewStatus -> IO [Bug]
listReviews' allopen status = do
  (session,user) <- bzLoginSession
  let reviews = reporterIs user .&&. packageReview
      open = if allopen then statusOpen else statusNewPost
      query = case status of
        ReviewAllOpen -> reviews .&&. statusOpen
        ReviewUnApproved -> reviews .&&. statusOpen .&&. not' reviewApproved
        ReviewUnbranched -> reviews .&&. statusNewModified .&&. reviewApproved
        _ -> reviews .&&. open .&&. reviewApproved
  -- FIXME sort by status, bid/pkg?
  bugs <- searchBugs session query
  case status of
    ReviewWithoutRepoReq ->
      filterM (fmap not . checkRepoRequestedComment session . bugId) bugs
    ReviewRepoRequested ->
      filterM (checkRepoRequestedComment session . bugId) bugs >>=
      filterM (fmap not . checkRepoCreatedComment session . bugId)
    ReviewRepoCreated ->
      filterM (checkRepoCreatedComment session . bugId) bugs
    ReviewUnbranched ->
      filterM (notBranched . reviewBugToPackage) bugs
    _ -> return bugs
  where
    checkRepoRequestedComment :: BugzillaSession -> BugId -> IO Bool
    checkRepoRequestedComment session bid =
        checkForComment session bid
          "https://pagure.io/releng/fedora-scm-requests/issue/"

    notBranched :: String -> IO Bool
    notBranched pkg = null <$> packagePagureBranched pkg

review :: String -> IO ()
review pkg = do
  (bugs, _) <- bugIdsSession $ pkgReviews pkg
  mapM_ putBugId bugs
