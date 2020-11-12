module Cmd.ListBranches (
  branchesCmd
  )
where

import Common
import Common.System

import Branches
import Git
import Package

-- FIXME remote/pagures branch and --remote or --no-remote
branchesCmd :: Bool -> Bool -> [String] -> IO ()
branchesCmd allbrs missing args = do
  (brs,pkgs) <- splitBranchesPkgs False Nothing args
  when allbrs $ do
    unless (null brs) $
      error' "cannot combine --all and branches"
    when missing $
      error' "cannot combine --all and --missing"
  if null pkgs
    then branchesPkg brs "."
    else mapM_ (branchesPkg brs) pkgs
  where
    branchesPkg :: [AnyBranch] -> FilePath -> IO ()
    branchesPkg branches path = do
      withExistingDirectory path $ do
        unlessM isPkgGitRepo $
          error' "not dist-git"
        pkg <- getPackageName path
        localbrs <- localBranches
        if allbrs then do
          putStr $ unPackage pkg ++ ": "
          putStrLn $ unwords localbrs
          else do
          if null branches then do
            -- FIXME better to filter inactive instead
            active <- getFedoraBranches
            let result = if missing then active \\ mapMaybe readBranch localbrs else activeBranches active localbrs
            putStr $ unPackage pkg ++ ": "
            putStrLn $ (unwords . map show) result
            else do
            let havebrs = filter (`elem` branches) (map anyBranch localbrs)
                result = if missing then branches \\ havebrs else havebrs
            unless (null result) $ do
              putStr $ unPackage pkg ++ ": "
              putStrLn $ (unwords . map show) result
