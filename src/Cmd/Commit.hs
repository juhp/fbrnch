module Cmd.Commit
  (commitPkgs,
   CommitOpt(..)) where

import Common
import Common.System
import Git
import Package

data CommitOpt = CommitMsg String | CommitAmend

commitPkgs :: Maybe CommitOpt -> [String] -> IO ()
commitPkgs mopt args =
  if null args
    then commitPkg "."
    else mapM_ commitPkg args
  where
    commitPkg :: FilePath -> IO ()
    commitPkg path =
      withExistingDirectory path $
      unlessM isGitDirClean $ do
      getPackageName path >>= putPkgHdr
      opts <- case mopt of
                Just opt -> return $
                  case opt of
                    CommitMsg msg -> ["-m", msg]
                    -- FIXME reject amend if already pushed
                    CommitAmend -> ["--amend", "--no-edit"]
                Nothing -> do
                  spec <- findSpecfile
                  -- FIXME check changelog in git diff
                  changelog <- getChangeLog spec
                  if length (lines changelog) > 1
                    then error' "spec changelog more than 1 line"
                    else return ["-m", changelog]
      git_ "commit" $ "-a" : opts
