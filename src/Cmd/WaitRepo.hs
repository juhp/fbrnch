module Cmd.WaitRepo (waitrepoCmd)
where

import Common.System

import Branches
import Git
import Koji
import Package

waitrepoCmd :: Bool -> (BranchesReq, [String]) -> IO ()
waitrepoCmd dryrun = do
  withPackageByBranches (Just False) cleanGitFetchActive AnyNumber (waitrepoBranch dryrun)

waitrepoBranch :: Bool -> Package -> AnyBranch -> IO ()
waitrepoBranch _ _ (OtherBranch _) =
  error' "waitrepo only defined for release branches"
waitrepoBranch dryrun pkg rbr@(RelBranch br) = do
  gitSwitchBranch rbr
  let spec = packageSpec pkg
  nvr <- pkgNameVerRel' br spec
  kojiWaitRepo dryrun (branchTarget br) nvr
