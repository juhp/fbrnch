module Pagure (
  srcfpo,
  pagureGroupInfo,
  pagureGroupRepos,
  pagureListGitBranches,
  pagureListProjectIssueTitlesStatus,
  pagureProjectInfo,
  pagureUserRepos,
  makeItem,
  pagurePRs
  ) where

import Common ((+/+))

import Fedora.Pagure
import Fedora.Pagure.Client (projectPRs, OutputFormat(FormatDefault))

srcfpo :: String
srcfpo = "src.fedoraproject.org"

-- Package makes circular dep
pagurePRs :: String -> IO ()
pagurePRs pkg =
  projectPRs srcfpo False False FormatDefault False Nothing Nothing Nothing ("rpms" +/+ pkg)
