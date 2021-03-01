module Cmd.Override (overrideCmd)
where

import Common
import Common.System

import Bodhi
import Branches
import Git
import Koji
import Package

overrideCmd :: Bool -> [String] -> IO ()
overrideCmd dryrun =
  withPackageByBranches (Just False) cleanGitFetchActive Nothing True AnyNumber overrideBranch
  where
    overrideBranch :: Package -> AnyBranch -> IO ()
    overrideBranch _ (OtherBranch _) =
      error' "override only defined for release branches"
    overrideBranch pkg rbr@(RelBranch br) = do
      putPkgBrnchHdr pkg br
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      putStrLn nvr
      tags <- kojiNVRTags nvr
      unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
        unlessM (checkAutoBodhiUpdate br) $
        if dryrun
        then putStrLn $ "override " ++ nvr
        else do
          bodhiCreateOverride nvr
          kojiWaitRepo (branchTarget br) nvr
