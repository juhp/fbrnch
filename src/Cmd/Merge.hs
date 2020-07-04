module Cmd.Merge (mergeCmd, mergeable, mergeBranch) where

import Common

import Data.Char

import Branches
import Git
import Package
import Prompt

mergeCmd :: ([Branch],[String]) -> IO ()
mergeCmd =
  withPackageByBranches False True LocalBranches runMergeBranch
  where
    runMergeBranch :: Package -> Branch -> IO ()
    runMergeBranch pkg br = do
      gitMergeOrigin br
      putPkgBrnchHdr pkg br
      gitSwitchBranch br
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
  mhash <-
    if newrepo && length unmerged == 1 then return $ Just ""
    else mergePrompt $ "Press Enter to merge " ++ (if build then "and build " else "") ++ show newerBr ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; or 'no' to skip merge"
  case mhash of
    Nothing -> return ()
    Just hash -> do
      let ref = if null hash then show newerBr else hash
      git_ "merge" ["--quiet", ref]
  where
    mergePrompt :: String -> IO (Maybe String)
    mergePrompt txt = do
      let commitrefs = map (head . words) unmerged
      inp <- prompt txt
      if null inp then return (Just "") else
        if map toLower inp == "no" then return Nothing
        else case find (inp `isPrefixOf`) commitrefs of
          Just ref -> return $ Just ref
          Nothing -> mergePrompt txt
