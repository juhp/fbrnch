module Cmd.WaitRepo (
  waitrepoCmd,
  WaitFetch(..)
  )
where

import Common.System

import Branches
import Git
import Koji
import Package
import Types

data WaitFetch = WaitNoFetch | WaitDirty | WaitFetch

-- FIXME first check/wait for build to actually exist
waitrepoCmd :: Bool -> WaitFetch -> Maybe SideTagTarget
            -> (BranchesReq, [String]) -> IO ()
waitrepoCmd dryrun fetch msidetagTarget = do
  withPackagesByBranches HeaderMay False
    (case fetch of
       WaitFetch -> cleanGitFetchActive
       WaitNoFetch -> cleanGitActive
       WaitDirty -> dirtyGitActive)
    AnyNumber waitrepoBranch
  where
    waitrepoBranch :: Package -> AnyBranch -> IO ()
    waitrepoBranch _ (OtherBranch _) =
      error' "waitrepo only defined for release branches"
    waitrepoBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      target <- targetMaybeSidetag dryrun True False br msidetagTarget
      kojiWaitRepoNVR dryrun False target nvr
