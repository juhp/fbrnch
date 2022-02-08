module Cmd.PullPush (pullPkgs, pushPkgs) where

import Branches
import Git
import Package

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: Bool -> (BranchesReq, [String]) -> IO ()
pullPkgs lenient =
  withPackageByBranches (Just False) (if lenient then Nothing else cleanGitFetch) AnyNumber pullPkg
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg pkg _br =
      if lenient
      then do
        haveGit <- isPkgGitRepo
        if haveGit
          then doPullPkg
          else putStrLn $ "ignoring " ++ unPackage pkg
      else doPullPkg
      where
        doPullPkg :: IO ()
        doPullPkg =
          getReleaseBranchWarn >>= gitMergeOrigin

pushPkgs :: (BranchesReq, [String]) -> IO ()
pushPkgs =
  withPackageByBranches (Just False) cleanGitFetch AnyNumber pushPkg
  where
    pushPkg :: Package -> AnyBranch -> IO ()
    pushPkg _pkg _br = do
      gitShortLog1 Nothing >>= putStrLn
      gitPushSilent Nothing
