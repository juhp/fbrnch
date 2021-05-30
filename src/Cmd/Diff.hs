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
-- FIXME --older/--newer branch
diffCmd :: Bool -> DiffWork -> DiffFormat -> Maybe AnyBranch
        -> (Maybe Branch,[String]) -> IO ()
diffCmd speconly work fmt mwbr =
  withPackagesMaybeBranch Nothing dirtyGit ZeroOrOne diffPkg
  where
    diffPkg :: Package -> AnyBranch -> IO ()
    diffPkg pkg br = do
      gitSwitchBranch br
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
          file = [packageSpec pkg | speconly]
      withBranch <-
        case mwbr of
          Nothing -> return []
          Just wbr ->
            let brn = show wbr in
              if '/' `elem` brn
              then return [brn]
              else do
                localbrs <- gitLines "branch" ["--format=%(refname:short)"]
                if brn `elem` localbrs
                  then return [brn]
                  else return ["origin/" ++ brn]
      let revdiff = case mwbr of
            Nothing -> []
            Just wbr -> case (wbr,br) of
              (RelBranch rwbr, RelBranch rbr) -> ["-R" | rwbr > rbr]
              _ -> []
      diff <- git "diff" $ contxt ++ workOpts ++ revdiff ++ withBranch ++ workArgs ++ file
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
