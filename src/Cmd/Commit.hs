module Cmd.Commit
  (commitPkgs,
   CommitOpt(..)) where

import Common
import Common.System
import Git
import Package

data CommitOpt = CommitMsg String | CommitAmend

commitPkgs :: Maybe CommitOpt -> [String] -> IO ()
commitPkgs mopt = mapM_ commitPkg
  where
    commitPkg :: String -> IO ()
    commitPkg pkg =
      withExistingDirectory pkg $
      unlessM isGitDirClean $ do
      putPkgHdr $ Package pkg
      opts <- case mopt of
                Just opt -> return $
                  case opt of
                    CommitMsg msg -> ["-m", msg]
                    CommitAmend -> ["--amend", "--no-edit"]
                Nothing -> do
                  spec <- findSpecfile
                  -- FIXME check changelog in git diff
                  -- FIXME change prompt to "commit" not "update"
                  changelog <- getChangeLog spec
                  if length (lines changelog) > 1
                    then error' "spec changelog more than 1 line"
                    else return ["-m", changelog]
      git_ "commit" $ "-a" : opts
