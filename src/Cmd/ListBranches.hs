module Cmd.ListBranches (
  branchesCmd
  )
where

import Common
import Common.System

import Branches
import Git
import Package

branchesCmd :: Bool -> [String] -> IO ()
branchesCmd allbrs args = do
  if null args
    then branchesPkg "."
    else mapM_ branchesPkg args
  where
    branchesPkg :: FilePath -> IO ()
    branchesPkg path = do
      withExistingDirectory path $ do
        pkg <- getPackageName path
        putPkgHdr pkg
        unlessM isPkgGitRepo $
          error' "not dist-git"
        if allbrs then do
          brs <- localBranches
          putStrLn $ unwords brs
          else do
          brs <- fedoraBranches localBranches
          putStrLn $ (unwords . map show) brs
