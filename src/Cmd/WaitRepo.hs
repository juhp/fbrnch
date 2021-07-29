module Cmd.WaitRepo (waitrepoCmd)
where

import Common.System
import Data.Maybe

import Branches
import Git
import Koji
import Package

waitrepoCmd :: Bool -> Maybe String -> (BranchesReq, [String]) -> IO ()
waitrepoCmd dryrun mtarget = do
  withPackageByBranches (Just False) cleanGitFetchActive AnyNumber waitrepoBranch
  where
    waitrepoBranch :: Package -> AnyBranch -> IO ()
    waitrepoBranch _ (OtherBranch _) =
      error' "waitrepo only defined for release branches"
    waitrepoBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      kojiWaitRepo dryrun (fromMaybe (branchTarget br) mtarget) nvr
