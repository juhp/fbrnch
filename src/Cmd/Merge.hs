module Cmd.Merge (mergeCmd, mergeable, mergeBranch) where

import Common
import Common.System

import Branches
import Git
import Package
import Prompt

-- add BranchOpts?
mergeCmd :: Bool -> (BranchesReq,[String]) -> IO ()
mergeCmd noprompt =
  withPackageByBranches (Just False) cleanGitFetchActive AnyNumber runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    runMergeBranch _pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      gitMergeOrigin br
      unmerged <- mergeable br
      mergeBranch False noprompt unmerged br

getNewerBranch :: Branch -> IO Branch
getNewerBranch Rawhide = return Rawhide
getNewerBranch br = do
  branches <- fedoraBranches (localBranches False)
  let newer = newerBranch br branches
  return $ if newer > br then newer
    -- FIXME this can be dropped with next fedora-dists
    else case elemIndex br branches of
           Just i -> branches !! (i - 1)
           Nothing -> error' $ show br ++ ": branch not found"

-- FIXME newer branch might not exist (eg epel8):
   -- restrict to local branches
mergeable :: Branch -> IO (Bool,[String])
mergeable br = do
  newer <- getNewerBranch br
  locals <- localBranches True
  gitMergeable (show newer `notElem` locals) newer

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> (Bool,[String]) -> Branch -> IO ()
mergeBranch _ _ _ Rawhide = return ()
mergeBranch _ _ (True,[]) _ = return ()
mergeBranch build noprompt (True, unmerged) br = do
  newerBr <- getNewerBranch br
  isnewrepo <- initialPkgRepo
  unless (null unmerged) $ do
    putStrLn $ (if isnewrepo || noprompt then "Merging from" else "New commits in") ++ " " ++ show newerBr ++ ":"
    mapM_ putStrLn unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ putStrLn unpushed
  -- FIXME avoid Mass_Rebuild bumps
  mmerge <-
    if isnewrepo && length unmerged == 1 || noprompt then return $ Just Nothing
    else refPrompt unmerged $ "Press Enter to merge " ++ show newerBr ++ (if build then " and build" else "") ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; or 'no' to skip merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ mhash -> do
    let ref = case mhash of
                Nothing -> show newerBr
                Just hash -> hash
    git_ "merge" ["--quiet", ref]
mergeBranch build noprompt (False,unmerged) br = do
  putStrLn "Branch is not directly mergeable:"
  mapM_ putStrLn unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ putStrLn unpushed
  mmerge <-
    if noprompt then return Nothing
    else conflictPrompt unmerged $ "Press Enter to skip merge" ++ (if build then " and build" else "") ++ "; or give a ref to attempt merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    git_ "merge" [ref]
