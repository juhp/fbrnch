module Cmd.Pull (pullPkgs) where

import Branches
import Git
import Package

pullPkgs :: [String] -> IO ()
pullPkgs = mapM_ pullPkg

pullPkg :: String -> IO ()
pullPkg pkg =
  withExistingDirectory pkg $ do
  checkWorkingDirClean
  br <- gitCurrentBranch
  gitMergeOrigin br
