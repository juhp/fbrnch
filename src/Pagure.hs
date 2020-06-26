module Pagure (
  srcfpo,
  pagureio,
  pagureProjectInfo,
  pagureListGitBranches,
  pagureListProjectIssueTitlesStatus,
  IssueTitleStatus(..),
  pagureUserRepos,
  makeItem,
  printScmIssue
  ) where

import qualified Common.Text as T

import Fedora.Pagure
import System.FilePath

srcfpo :: String
srcfpo = "src.fedoraproject.org"

pagureio :: String
pagureio = "pagure.io"

printScmIssue :: IssueTitleStatus -> IO ()
printScmIssue issue =
  putStrLn $ "https://" ++ pagureio </> "releng/fedora-scm-requests" </> "issue" </> show (pagureIssueId issue) ++ " (" ++ T.unpack (pagureIssueStatus issue) ++ mclosed ++ "): " ++ pagureIssueTitle issue
  where
    mclosed = maybe "" (\s-> ":" ++ T.unpack s) $ pagureIssueCloseStatus issue
