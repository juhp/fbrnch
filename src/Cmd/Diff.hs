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

-- FIXME diff other branches without switching
diffCmd :: DiffWork -> DiffFormat -> Maybe AnyBranch -> [String] -> IO ()
diffCmd work fmt mwbr =
  withPackageByBranches Nothing dirtyGit Nothing zeroOneBranches diffPkg
  where
    diffPkg :: Package -> AnyBranch -> IO ()
    diffPkg pkg br = do
      let contxt = case fmt of
                     DiffContext n -> ["--unified=" ++ show n]
                     DiffMinimal -> ["--unified=0"]
                     -- FIXME hide "files changed" and "insertions" summary
                     DiffStats -> ["--compact-summary"]
                     _ -> []
          (workOpts,workArgs) =
            case work of
              DiffWorkAll -> ([],["HEAD" | isNothing mwbr])
              DiffWorkUnstage -> ([],[])
              DiffWorkStaged -> (["--cached"],[])
          withBranch = case mwbr of
                         Nothing -> []
                         Just wbr -> [show wbr]
      diff <- git "diff" $ contxt ++ workOpts ++ withBranch ++ workArgs
      unless (null diff) $
        case fmt of
          DiffQuiet -> putStrLn $ unPackage pkg
          DiffMinimal -> do
            putPkgAnyBrnchHdr pkg br
            putStr $ minifyDiff diff
          _ -> do
            putPkgAnyBrnchHdr pkg br
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
