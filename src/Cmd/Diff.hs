module Cmd.Diff (
  diffCmd,
  DiffFormat(..),
  DiffWork(..)
  ) where

import Branches
import Common
import Common.System
import Git
import Package

data DiffFormat =
  DiffDefault | DiffQuiet | DiffContext Int | DiffMinimal | DiffStats
  deriving (Eq)

data DiffWork =
  DiffWorkAll | DiffWorkUnstage | DiffWorkStaged
  deriving (Eq)

-- FIXME diff branch(es) (without switching?)
-- FIXME --no-fetch
diffCmd :: DiffWork -> DiffFormat -> Branch -> [String] -> IO ()
diffCmd work fmt br pkgs =
  withPackageByBranches False dirtyGit diffPkg (BranchList [br],pkgs)
  where
    diffPkg :: Package -> Branch -> IO ()
    diffPkg pkg _br = do
      let contxt = case fmt of
                     DiffContext n -> ["--unified=" ++ show n]
                     DiffMinimal -> ["--unified=0"]
                     -- FIXME hide "files changed" and "insertions" summary
                     DiffStats -> ["--compact-summary"]
                     _ -> []
          workArgs = case work of
                       DiffWorkAll -> ["HEAD"]
                       DiffWorkUnstage -> []
                       DiffWorkStaged -> ["--cached"]
      diff <- git "diff" $ contxt ++ workArgs
      unless (null diff) $
        case fmt of
          DiffQuiet -> putStrLn $ unPackage pkg
          DiffMinimal -> do
            putPkgBrnchHdr pkg br
            putStr $ minifyDiff diff
          _ -> do
            putPkgBrnchHdr pkg br
            putStrLn diff
        where
          minifyDiff =
            unlines . maybeRemoveDiffGit . filterCommon . lines
            where
              filterCommon =
                -- flist is from swish
                let flist fs a = map ($ a) fs in
                filter (not . or . flist (map isPrefixOf ["--- a/", "+++ b/", "index ", "@@ -"]))

              maybeRemoveDiffGit ls =
                let spec = unPackage pkg <.> "spec"
                    specDiffGit = "diff --git a/" ++ spec ++ " b/" ++ spec
                    gitDiffs = filter ("diff --git a/" `isPrefixOf`) ls in
                  if gitDiffs == [specDiffGit]
                  then delete specDiffGit ls
                  else ls
