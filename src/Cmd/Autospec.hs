module Cmd.Autospec (
  autospecCmd
  )
where

import Control.Monad.Extra (unlessM)
import SimpleCmd (cmd, cmd_)
import System.Directory (doesFileExist)

import Branches
import Git
import Package

-- FIXME calculate baserelease
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
          git_ "commit" ["-m", "refresh changelog"]
      else putStrLn "'changelog' file already exists"
      else cmd_ "rpmautospec" ["convert"]
