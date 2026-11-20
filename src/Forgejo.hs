module Forgejo (
  readIssue,
  IssueTitleStatus(..),
  printScmIssue,
  hasIssueLabel
  )
where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Gitea.Forgejo
import Common ((+/+), (+-+))

data IssueTitleStatus =
  IssueTitleStatus { forgeIssueId :: Integer
                   , forgeIssueTitle :: String
                   , forgeIssueStatus :: T.Text
                   , forgeIssueLabels :: Maybe [T.Text]
                   }

readIssue :: Issue -> Maybe IssueTitleStatus
readIssue issue = do
  number <- issueNumber issue
  title <- T.unpack <$> issueTitle issue
  state <- issueState issue
  let labels = mapMaybe labelName <$> issueLabels issue
  return $ IssueTitleStatus number title state labels

printScmIssue :: IssueTitleStatus -> IO ()
printScmIssue issue =
  putStrLn $ "https://forge.fedproject.org" +/+ "releng/fedora-scm-requests" +/+ "issue" +/+ show (forgeIssueId issue) +-+ "(" ++ T.unpack (forgeIssueStatus issue) ++ labels ++ "):" +-+ forgeIssueTitle issue
  where
    labels = maybe "" show $ forgeIssueLabels issue

hasIssueLabel :: String -> IssueTitleStatus -> Bool
hasIssueLabel label issue =
  case forgeIssueLabels issue of
    Just labels -> T.pack label `elem` labels
    Nothing -> False
