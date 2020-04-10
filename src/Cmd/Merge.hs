module Cmd.Merge (mergeCmd, mergeBranch) where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Fedora.Branch

import Branches
import Git
import Package
import Prompt
import Types

mergeCmd :: Maybe Branch -> [Package] -> IO ()
mergeCmd mbr pkgs =
  if null pkgs
  then do
    branches <- case mbr of
                  Just b -> return [b]
                  Nothing -> packageBranches
    mapM_ (mergeBranch False False Nothing) branches
  else mapM_ (mergePkg mbr) pkgs

mergePkg :: Maybe Branch -> Package -> IO ()
mergePkg mbr pkg =
  withExistingDirectory pkg $ do
    checkWorkingDirClean
    git_ "fetch" []
    branches <- case mbr of
                  Just b -> return [b]
                  Nothing -> packageBranched
    when (isNothing mbr) $
      putStrLn $ "\nBranches: " ++ unwords (map show branches)
    mapM_ (mergeBranch False False (Just pkg)) branches

mergeBranch :: Bool -> Bool -> Maybe Package -> Branch -> IO ()
mergeBranch pulled build mpkg br = do
  pkg <- getPackageName mpkg
  unless pulled $ do
    checkWorkingDirClean
    gitPull
  unless build $ do
    putPkgBrnchHdr pkg br
    switchBranch br
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
        -- FIXME ignore Mass_Rebuild?
    when (ancestor && not (null unmerged)) $ do
      -- FIXME if only initial README commit then package can't be built without merge
      mhash <-
        if newrepo then return ""
        else prompt $ "to merge " ++ show prev ++ (if length unmerged > 1 then "; or give a ref to merge" else "") ++ "; otherwise 'no' to skip merge"
      -- FIXME really check for "no"?
      let commitrefs = map (head . words) unmerged
          mref = find (mhash `isPrefixOf`) commitrefs
      when (null mhash || isJust mref) $ do
        let ref = if null mhash
                  then show prev
                  else fromJust mref
        git_ "merge" ["--quiet", ref]
