module Pagure (
  srcfpo,
  pagureio,
  pagureProjectInfo,
  pagureListGitBranches,
  pagureListProjectIssueTitles,
  pagureUserRepos,
  makeItem,
  printScmIssue,
  snd3
  ) where

import qualified Common.Text as T

import Fedora.Pagure
import System.FilePath

srcfpo :: String
srcfpo = "src.fedoraproject.org"

pagureio :: String
pagureio = "pagure.io"

printScmIssue :: (Integer, String, T.Text) -> IO ()
printScmIssue (issue,title,status) =
  putStrLn $ "https://" ++ pagureio </> "releng/fedora-scm-requests" </> "issue" </> show issue ++ " (" ++ T.unpack status ++ "): " ++ title

snd3 :: (a,b,c) -> b
snd3 (_,t,_) = t
