module Cmd.Override (overrideCmd)
where

import Common
import Common.System

import Bodhi
import Branches
import Git
import Koji
import Package

-- FIXME option to expire (all) overrides
overrideCmd :: Bool -> Maybe Int -> [Branch] -> [String] -> IO ()
overrideCmd dryrun mduration brs pkgs =
  withPackageByBranches (Just False) cleanGitFetchActive AnyNumber overrideBranch (Branches brs, pkgs)
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
        unlessM (checkAutoBodhiUpdate br) $ do
        bodhiCreateOverride dryrun mduration nvr
        kojiWaitRepo dryrun (branchTarget br) nvr
