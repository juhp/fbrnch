module Pagure (
  srcfpo,
  pagureio,
  pagureProjectInfo,
  pagureListGitBranches,
  pagureListProjectIssueTitles,
  makeItem,
  printScmIssue,
  snd3
  ) where

import qualified Common.Text as T
import System.FilePath
import Web.Fedora.Pagure

srcfpo :: String
srcfpo = "src.fedoraproject.org"

pagureio :: String
pagureio = "pagure.io"

printScmIssue :: (Integer, String, T.Text) -> IO ()
printScmIssue (issue,title,status) =
  putStrLn $ "https://" <> pagureio </> "releng/fedora-scm-requests" </> "issue" </> show issue <> " (" <> T.unpack status <> "): " <> title

snd3 :: (a,b,c) -> b
snd3 (_,t,_) = t
