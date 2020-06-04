module Cmd.Merge (mergeCmd, mergeable, mergeBranch) where

import Common

import Data.Char

import Branches
import Git
import Package
import Prompt

mergeCmd :: ([Branch],[Package]) -> IO ()
mergeCmd =
  withPackageBranches LocalBranches runMergeBranch
  where
    runMergeBranch :: Package -> Branch -> IO ()
    runMergeBranch pkg br = do
      gitMergeOrigin br
      putPkgBrnchHdr pkg br
      gitSwitchBranch br
      unmerged <- mergeable br
      mergeBranch unmerged br

mergeable :: Branch -> IO [String]
mergeable br = do
  newerBr <- do
    branches <- getFedoraBranches
    return $ newerBranch branches br
  gitMergeable $ show newerBr

mergeBranch :: [String] -> Branch -> IO ()
mergeBranch _ Master = return ()
mergeBranch [] _ = return ()
mergeBranch unmerged br = do
  newerBr <- do
    branches <- getFedoraBranches
    return $ newerBranch branches br
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
    if newrepo then return $ Just ""
    else mergePrompt $ "Press Enter to merge " ++ show newerBr ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; or 'no' to skip merge"
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
