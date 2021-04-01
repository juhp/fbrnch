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
-- FIXME --local for existing local branches
branchesCmd :: Bool -> Bool -> Bool -> Bool -> (BranchesReq,[String]) -> IO ()
branchesCmd skipdead allbrs missing remote (breq, pkgs) = do
  -- when (allbrs $ do
  --   unless (null brs) $
  --     error' "cannot combine --all and branches"
  --   when missing $
  --     error' "cannot combine --all and --missing"
  if null pkgs
    then branchesPkg "."
    else mapM_ branchesPkg pkgs
  where
    branchesPkg :: FilePath -> IO ()
    branchesPkg path = do
      if remote
        then doBranchesPkg
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
          unless remote $
            unlessM isPkgGitRepo $
            error' "not dist-git"
          pkg <- getPackageName path
          brs <- delete "main" <$>
                 if remote
                 then pagurePkgBranches (unPackage pkg)
                 else localBranches
          if allbrs then do
            putStrLn $ unPackage pkg ++ ": " ++ unwords brs
            else do
            if breq == Branches [] then do
              -- FIXME better to filter inactive instead
              active <- getFedoraBranches
              let result = if missing then active \\ mapMaybe readBranch brs else activeBranches active brs
              putStr $ unPackage pkg ++ ": "
              putStrLn $ (unwords . map show) result
              else do
              branches <- listOfBranches True False breq
              let havebrs = filter (`elem` branches) $ map readBranch' brs
                  result = if missing then branches \\ havebrs else havebrs
              unless (null result) $ do
                putStr $ unPackage pkg ++ ": "
                putStrLn $ (unwords . map show) result
