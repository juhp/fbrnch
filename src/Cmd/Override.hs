module Cmd.Override (overrideCmd)
where

import Common
import Common.System

import Bodhi
import Branches
import Cmd.WaitRepo (waitrepoCmd)
import Git
import Koji
import Package

-- FIXME option to expire (all) overrides
overrideCmd :: Bool -> Maybe Int -> Bool -> (BranchesReq, [String]) -> IO ()
overrideCmd dryrun mduration nowait breqpkgs = do
  unless nowait $
    putStrLn "Overriding"
  withPackageByBranches (Just False) cleanGitFetchActive AnyNumber overrideBranch breqpkgs
  unless nowait $ do
    putStrLn "Waiting"
    waitrepoCmd False dryrun Nothing breqpkgs
  where
    overrideBranch :: Package -> AnyBranch -> IO ()
    overrideBranch _ (OtherBranch _) =
      error' "override only defined for release branches"
    overrideBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      putStrLn nvr
      tags <- kojiNVRTags nvr
      unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
        unlessM (checkAutoBodhiUpdate br) $ do
        bodhiCreateOverride dryrun mduration nvr
