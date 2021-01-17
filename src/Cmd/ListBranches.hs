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
branchesCmd :: Bool -> Bool -> Bool -> Bool -> [String] -> IO ()
branchesCmd skipdead allbrs missing remote args = do
  (brs,pkgs) <- splitBranchesPkgs False Nothing (not remote) args
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
      if remote then doBranchesPkg
        else
        withExistingDirectory path $
        if skipdead then
          ifM (doesFileExist "dead.package")
          (return ())
          doBranchesPkg
        else doBranchesPkg
      where
        doBranchesPkg :: IO ()
        doBranchesPkg = do
          unlessM isPkgGitRepo $
            unless remote $
            error' "not dist-git"
          pkg <- getPackageName path
          brs <- if remote
                 then pagurePkgBranches (unPackage pkg)
                 else localBranches
          if allbrs then do
            putStrLn $ unPackage pkg ++ ": " ++ unwords brs
            else do
            if null branches then do
              -- FIXME better to filter inactive instead
              active <- getFedoraBranches
              let result = if missing then active \\ mapMaybe readBranch brs else activeBranches active brs
              putStr $ unPackage pkg ++ ": "
              putStrLn $ (unwords . map show) result
              else do
              let havebrs = filter (`elem` branches) (map anyBranch brs)
                  result = if missing then branches \\ havebrs else havebrs
              unless (null result) $ do
                putStr $ unPackage pkg ++ ": "
                putStrLn $ (unwords . map show) result
