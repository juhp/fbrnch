module Cmd.Merge (mergeCmd, mergeBranch) where

import Common

import Data.Char

import Branches
import Git
import Package
import Prompt

mergeCmd :: ([Branch],[Package]) -> IO ()
mergeCmd =
  withPackageBranches True runMergeBranch
  where
    runMergeBranch :: Maybe Package -> Branch -> IO ()
    runMergeBranch mpkg br = do
      pkg <- getPackageName mpkg
      checkWorkingDirClean
      gitPull
      putPkgBrnchHdr pkg br
      switchBranch br
      mergeBranch br

mergeBranch :: Branch -> IO ()
mergeBranch br = do
  prev <- do
    branches <- getFedoraBranches
    return $ newerBranch branches br
  unless (br == Master) $ do
    ancestor <- gitBool "merge-base" ["--is-ancestor", "HEAD", show prev]
    unmerged <- gitShortLog $ "HEAD.." ++ show prev
    newrepo <- initialPkgRepo
    when (ancestor && not newrepo) $
      unless (null unmerged) $ do
        putStrLn $ "New commits in " ++ show prev ++ ":"
        mapM_ (putStrLn . simplifyCommitLog) unmerged
    unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
    unless (null unpushed) $ do
      putStrLn "Local commits:"
      mapM_ (putStrLn . simplifyCommitLog) unpushed
    -- FIXME avoid Mass_Rebuild bumps
    when (ancestor && not (null unmerged)) $ do
      mhash <-
        if newrepo then return Nothing
        else mergePrompt unmerged $ "Press Enter to merge " ++ show prev ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; otherwise 'no' to skip merge"
      case mhash of
        Nothing -> return ()
        Just hash -> do
          let ref = if null hash then show prev else hash
          git_ "merge" ["--quiet", ref]
  where
    mergePrompt :: [String] -> String -> IO (Maybe String)
    mergePrompt unmerged txt = do
      let commitrefs = map (head . words) unmerged
      inp <- prompt txt
      if null inp then return (Just "") else
        if map toLower inp == "no" then return Nothing
        else case find (inp `isPrefixOf`) commitrefs of
          Just ref -> return $ Just ref
          Nothing -> mergePrompt unmerged txt
