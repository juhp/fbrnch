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

import System.IO.Extra

-- FIXME --force
-- FIXME --target
bumpPkgs :: Bool -> Maybe CommitOpt -> (BranchesReq,[String]) -> IO ()
bumpPkgs local mopt =
  withPackageByBranches (Just local) (if local then cleanGit else cleanGitFetchActive)
  AnyNumber bumpPkg
  where
    bumpPkg :: Package -> AnyBranch -> IO ()
    bumpPkg pkg br = do
      dead <- doesFileExist "dead.package"
      if dead
        then putStrLn "dead package"
        else do
        spec <- localBranchSpecFile pkg br
        rbr <-
          case br of
            RelBranch rbr -> return rbr
            OtherBranch _ -> systemBranch
        newnvr <- pkgNameVerRel' rbr spec
        moldnvr <-
          if local
          then do
            withTempFile $ \tempfile -> do
              git "show" ["origin:" ++ spec] >>= writeFile tempfile
              pkgNameVerRel rbr tempfile
          else
            case br of
              RelBranch rbr' ->
                let tag = branchDestTag rbr' in
                  kojiLatestNVR tag $ unPackage pkg
              -- FIXME fallback to local?
              _ -> return Nothing
        if equivNVR newnvr (fromMaybe "" moldnvr) then do
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
          -- FIXME quiet commit?
          git_ "commit" $ "-a" : copts
          else putStrLn "already bumped"
