module Cmd.Override (overrideCmd)
where

import Common
import Common.System

import Bodhi
import Branches
import Git
import Koji
import Package

overrideCmd :: [String] -> IO ()
overrideCmd =
  withPackageByBranches (Just False) cleanGitFetchActive Nothing Nothing overrideBranch
  where
    overrideBranch :: Package -> AnyBranch -> IO ()
    overrideBranch _ (OtherBranch _) =
      error' "override only defined for release branches"
    overrideBranch pkg rbr@(RelBranch br) = do
      putPkgBrnchHdr pkg br
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      nvr <- pkgNameVerRel' br spec
      putStrLn $ nvr
      mtags <- kojiNVRTags nvr
      case mtags of
        Nothing -> error' $ nvr ++ " is untagged"
        Just tags ->
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
          unlessM (checkAutoBodhiUpdate br) $ do
          bodhiCreateOverride nvr
          kojiWaitRepo (branchTarget br) nvr
