module Cmd.WaitRepo (waitrepoCmd)
where

import Common.System
import Data.Maybe

import Branches
import Git
import Koji
import Package

-- FIXME first check/wait for build to actually exist
waitrepoCmd :: Bool -> Bool -> Maybe String -> (BranchesReq, [String]) -> IO ()
waitrepoCmd fetch dryrun mtarget = do
  withPackageByBranches (Just False) (if fetch then cleanGitFetchActive else cleanGitActive) AnyNumber waitrepoBranch
  where
    waitrepoBranch :: Package -> AnyBranch -> IO ()
    waitrepoBranch _ (OtherBranch _) =
      error' "waitrepo only defined for release branches"
    waitrepoBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      kojiWaitRepo dryrun (fromMaybe (branchTarget br) mtarget) nvr
