module Cmd.Commit
  (commitPkgs,
   CommitOpt(..)) where

import Common
import Git
import Package

data CommitOpt = CommitMsg String | CommitAmend

-- FIXME take commit msg from changelog
commitPkgs :: CommitOpt -> [String] -> IO ()
commitPkgs opt = mapM_ commitPkg
  where
    commitPkg :: String -> IO ()
    commitPkg pkg =
      withExistingDirectory pkg $
      unlessM isGitDirClean $ do
      putPkgHdr $ Package pkg
      let opts = case opt of
                   CommitMsg msg -> ["-m", msg]
                   CommitAmend -> ["--amend", "--no-edit"]
      git_ "commit" $ "-a" : opts
