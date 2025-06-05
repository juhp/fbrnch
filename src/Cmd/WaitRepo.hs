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
-- FIXME check on previous/origin commit/build
waitrepoCmd :: Bool -> Bool -> WaitFetch -> Maybe SideTagTarget
            -> (BranchesReq, [String]) -> IO ()
waitrepoCmd dryrun noNvr fetch msidetagTarget (breq,pkgs) = do
  -- FIXME detect sidetag so not mistaken for pkg
  if noNvr
    then
    if null pkgs
    then withPackagesByBranches HeaderMay False Nothing AnyNumber waitrepoBranch (breq,["."])
    else error' "cannot specify pkgs with --no-nvr"
    else withPackagesByBranches HeaderMay False
         (case fetch of
            WaitFetch -> cleanGitFetchActive
            WaitNoFetch -> cleanGitActive
            WaitDirty -> dirtyGitActive)
         AnyNumber waitrepoBranchPkg (breq,pkgs)
  where
    waitrepoBranchPkg :: Package -> AnyBranch -> IO ()
    waitrepoBranchPkg pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      target <- targetMaybeSidetag dryrun True False br msidetagTarget
      kojiWaitRepoNVR dryrun False target nvr
    waitrepoBranchPkg _ (OtherBranch _) =
      error' "waitrepo only defined for release branches"

    waitrepoBranch :: Package -> AnyBranch -> IO ()
    waitrepoBranch _ (RelBranch br) = do
      target <- targetMaybeSidetag dryrun True False br msidetagTarget
      kojiWaitRepo dryrun False target
    waitrepoBranch _ (OtherBranch _) =
      error' "waitrepo only defined for release branches"
