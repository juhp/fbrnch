module Cmd.Merge (mergeCmd, mergeable, mergeBranch) where

import Common
import Common.System

import Branches
import Git
import Package
import Prompt

mergeCmd :: (Branches,[String]) -> IO ()
mergeCmd =
  withPackageByBranches True cleanGitFetch runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    runMergeBranch pkg rbr@(RelBranch br) = do
      putPkgBrnchHdr pkg br
      gitSwitchBranch rbr
      gitMergeOrigin rbr
      unmerged <- mergeable br
      mergeBranch False unmerged br

-- FIXME newer branch might not exist (eg epel8):
   -- restrict to local branches
mergeable :: Branch -> IO [String]
mergeable br = do
  newerBr <- newerBranch br <$> getFedoraBranches
  gitMergeable $ show newerBr

-- FIXME return merged ref
mergeBranch :: Bool -> [String] -> Branch -> IO ()
mergeBranch _ _ Master = return ()
mergeBranch _ [] _ = return ()
mergeBranch build unmerged br = do
  newerBr <- newerBranch br <$> getFedoraBranches
  newrepo <- initialPkgRepo
  unless (null unmerged) $ do
    putStrLn $ (if newrepo then "Merging from" else "New commits in") ++ " " ++ show newerBr ++ ":"
    mapM_ (putStrLn . simplifyCommitLog) unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ (putStrLn . simplifyCommitLog) unpushed
  -- FIXME avoid Mass_Rebuild bumps
  mmerge <-
    if newrepo && length unmerged == 1 then return $ Just Nothing
    else refPrompt unmerged $ "Press Enter to merge " ++ (if build then "and build " else "") ++ show newerBr ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; or 'no' to skip merge"
  whenJust mmerge $ \ mhash -> do
    let ref = case mhash of
                Nothing -> show newerBr
                Just hash -> hash
    git_ "merge" ["--quiet", ref]
