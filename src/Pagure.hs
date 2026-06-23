module Pagure (
  srcfpo,
  pagureio,
  pagureGroupInfo,
  pagureGroupRepos,
  pagureListGitBranches,
  pagureListProjectIssueTitlesStatus,
  IssueTitleStatus(..),
  pagureProjectInfo,
  pagureUserRepos,
  makeItem,
  printScmIssue,
  pagurePRs
  ) where

import Common ((+/+), (+-+))
import qualified Common.Text as T

import Fedora.Pagure
import Fedora.Pagure.Client (projectPRs, OutputFormat(FormatDefault))

srcfpo :: String
srcfpo = "src.fedoraproject.org"

pagureio :: String
pagureio = "pagure.io"

printScmIssue :: IssueTitleStatus -> IO ()
printScmIssue issue =
  putStrLn $ "https://" ++ pagureio +/+ "releng/fedora-scm-requests" +/+ "issue" +/+ show (pagureIssueId issue) +-+ "(" ++ T.unpack (pagureIssueStatus issue) ++ mclosed ++ "):" +-+ pagureIssueTitle issue
  where
    mclosed = maybe "" (\s-> ":" ++ T.unpack s) $ pagureIssueCloseStatus issue

-- Package makes circular dep
pagurePRs :: String -> IO ()
pagurePRs pkg =
  projectPRs srcfpo False False FormatDefault False Nothing Nothing Nothing ("rpms" +/+ pkg)
