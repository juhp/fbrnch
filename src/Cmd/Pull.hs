module Cmd.Pull (pullPkgs) where

import Git
import Package

pullPkgs :: [Package] -> IO ()
pullPkgs = mapM_ pullPkg

pullPkg :: String -> IO ()
pullPkg pkg =
  withExistingDirectory pkg $ do
  checkWorkingDirClean
  br <- gitCurrentBranch
  gitMergeOrigin br
