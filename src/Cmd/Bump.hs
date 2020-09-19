module Cmd.Bump
  ( bumpPkgs,
  )
where

import Branches
import Common.System
import Git
import Package

-- FIXME check koji if bump needed
bumpPkgs :: Maybe CommitOpt -> Maybe BranchOpts -> [String] -> IO ()
bumpPkgs mopt mbrnchopts =
  withPackageByBranches (Just True) cleanGitFetchActive mbrnchopts Nothing bumpPkg
  where
    bumpPkg :: Package -> AnyBranch -> IO ()
    bumpPkg pkg br = do
      -- latest <- kojiLatestPkg dist pkg
      -- when (eqNVR nvr latest) $ do
      spec <- localBranchSpecFile pkg br
      git_ "log" ["origin..HEAD", "--pretty=oneline"]
      let clmsg =
            case mopt of
              Just (CommitMsg msg) -> msg
              _ -> "rebuild"
      cmd_ "rpmdev-bumpspec" ["-c", clmsg, spec]
      let copts =
            case mopt of
              Nothing -> ["-m", "bump release"]
              Just opt ->
                case opt of
                  CommitMsg msg -> ["-m", msg]
                  -- FIXME reject amend if already pushed
                  CommitAmend -> ["--amend", "--no-edit"]
      git_ "commit" $ "-a" : copts
