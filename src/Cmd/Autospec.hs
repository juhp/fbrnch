module Cmd.Autospec (
  autospecCmd,
  unautospecCmd
  )
where

import Control.Monad.Extra (unlessM, when)
import SimpleCmd (cmd, cmd_)
import System.Directory (doesFileExist, removeFile)

import Branches
import Git
import Package

-- FIXME! calculate baserelease: calculate bumped release and count back to last version bump commit
autospecCmd :: Bool -> [String] -> IO ()
autospecCmd force pkgs =
  withPackagesByBranches HeaderMay False cleanGitFetchActive ExactlyOne autospecPkg (Branches [Rawhide], pkgs)
  where
  autospecPkg :: Package -> AnyBranch -> IO ()
  autospecPkg _pkg br = do
    gitSwitchBranch br
    let changelogfile = "changelog"
    exists <- doesFileExist changelogfile
    if exists
      then
      if force
      then do
        cmd "rpmautospec" ["generate-changelog"] >>=
          writeFile changelogfile
        unlessM (null <$> git "status" ["--porcelain", "--untracked=no"]) $ do
          git_ "add" [changelogfile]
          git_ "commit" ["-m", "Refresh changelog"]
      else putStrLn "'changelog' file already exists"
      else cmd_ "rpmautospec" ["convert"]

unautospecCmd :: (BranchesReq, [String]) -> IO ()
unautospecCmd =
  withPackagesByBranches HeaderMay False cleanGitFetchActive ExactlyOne unautospecPkg
  where
  unautospecPkg :: Package -> AnyBranch -> IO ()
  unautospecPkg pkg br = do
    spec <- localBranchSpecFile pkg br
    autorelease <- isAutoRelease spec
    autochange <- isAutoChangelog spec
    when autochange $ do
      changelog <- cmd "rpmautospec" ["generate-changelog", spec]
      cmd_ "sed" ["-i", "/%autochangelog/d", spec]
      appendFile spec $ "%changelog\n" ++ changelog ++ "\n"
      removeFile "changelog"
    when autorelease $ do
      release <- calculateRelease spec
      cmd_ "sed" ["-i", "s/%autorelease/" ++ release ++ "%{?dist}/", spec]
