module Cmd.WaitRepo (
  waitrepoCmd,
  WaitFetch(..)
  )
where

import Common.System

import Branches
import Common (showNVR)
import Git
import Koji
import Package
import Types

data WaitFetch = WaitNoFetch | WaitDirty | WaitFetch

-- FIXME first check/wait for build to actually exist
waitrepoCmd :: Bool -> Bool -> WaitFetch -> Maybe SideTagTarget
            -> (BranchesReq, [String]) -> IO ()
waitrepoCmd dryrun knowntag fetch msidetagTarget = do
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
      target <- targetMaybeSidetag dryrun False br msidetagTarget
      logMsg $ "Waiting for" +-+ showNVR nvr +-+ "to appear in" +-+ target
      -- FIXME can we get time from koji waitrepo task?
      timeIO $ kojiWaitRepoNVR dryrun True knowntag target nvr
