module Cmd.Merge (mergeCmd, mergeable, mergeBranch) where

import Common
import Common.System

import Branches
import Git
import Package
import Prompt

-- FIXME add --no-prompt
-- add BranchOpts?
mergeCmd :: [String] -> IO ()
mergeCmd =
  withPackageByBranches (Just False) cleanGitFetchActive Nothing True AnyNumber runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    runMergeBranch pkg rbr@(RelBranch br) = do
      putPkgBrnchHdr pkg br
      gitSwitchBranch rbr
      gitMergeOrigin rbr
      unmerged <- mergeable br
      mergeBranch False False unmerged br

getNewerBranch :: Branch -> IO Branch
getNewerBranch Master = return Master
getNewerBranch br = do
  branches <- fedoraBranches localBranches
  let newer = newerBranch br branches
  return $ if newer > br then newer
    -- FIXME this can be dropped with next fedora-dists
    else case elemIndex br branches of
           Just i -> branches !! (i - 1)
           Nothing -> error' $ show br ++ ": branch not found"

-- FIXME newer branch might not exist (eg epel8):
   -- restrict to local branches
mergeable :: Branch -> IO [String]
mergeable br = do
  newerBr <- getNewerBranch br
  gitMergeable $ show newerBr

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> [String] -> Branch -> IO ()
mergeBranch _ _ _ Master = return ()
mergeBranch _ _ [] _ = return ()
mergeBranch build noprompt unmerged br = do
  newerBr <- getNewerBranch br
  newrepo <- initialPkgRepo
  unless (null unmerged) $ do
    putStrLn $ (if newrepo || noprompt then "Merging from" else "New commits in") ++ " " ++ show newerBr ++ ":"
    mapM_ (putStrLn . simplifyCommitLog) unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ (putStrLn . simplifyCommitLog) unpushed
  -- FIXME avoid Mass_Rebuild bumps
  mmerge <-
    if newrepo && length unmerged == 1 || noprompt then return $ Just Nothing
    else refPrompt unmerged $ "Press Enter to merge " ++ show newerBr ++ (if build then " and build" else "") ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; or 'no' to skip merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ mhash -> do
    let ref = case mhash of
                Nothing -> show newerBr
                Just hash -> hash
    git_ "merge" ["--quiet", ref]
