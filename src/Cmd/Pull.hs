module Cmd.Pull (pullPkgs) where

import Git
import Package
import Types

pullPkgs :: [Package] -> IO ()
pullPkgs = mapM_ pullPkg

pullPkg :: String -> IO ()
pullPkg pkg =
  withExistingDirectory pkg $ do
    checkWorkingDirClean
    git_ "pull" ["--rebase"]
