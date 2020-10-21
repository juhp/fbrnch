module Cmd.Bump
  ( bumpPkgs,
  )
where

import Branches
import Common
import Common.System
import Git
import Koji
import Package

bumpPkgs :: Maybe CommitOpt -> Maybe BranchOpts -> [String] -> IO ()
bumpPkgs mopt mbrnchopts =
  withPackageByBranches (Just True) cleanGitFetchActive mbrnchopts AnyNumber bumpPkg
  where
    bumpPkg :: Package -> AnyBranch -> IO ()
    bumpPkg pkg br = do
      -- FIXME or check for unpushed bump
      mlatest <-
        case br of
          RelBranch rbr ->
            let tag = branchDestTag rbr in
              kojiLatestNVR tag $ unPackage pkg
          _ -> return Nothing
      spec <- localBranchSpecFile pkg br
      rbr <-
        case br of
          RelBranch rbr -> return rbr
          OtherBranch _ -> systemBranch
      nvr <- pkgNameVerRel' rbr spec
      if equivNVR nvr (fromMaybe "" mlatest) then do
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
        else putStrLn "already bumped"
