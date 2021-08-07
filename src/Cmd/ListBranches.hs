module Cmd.ListBranches (
  branchesCmd,
  BranchesMode(..)
  )
where

import Common
import Common.System

import Branches
import Git
import Package

data BranchesMode = Local | Remote | Current
  deriving Eq

-- FIXME remote/pagures branch and --remote or --no-remote
-- FIXME --local for existing local branches
branchesCmd :: Bool -> Bool -> Bool -> BranchesMode -> (BranchesReq,[String])
            -> IO ()
branchesCmd skipdead allbrs missing mode (breq, pkgs) = do
  -- when (allbrs $ do
  --   unless (null brs) $
  --     error' "cannot combine --all and branches"
  --   when missing $
  --     error' "cannot combine --all and --missing"
  when (mode == Current) $
    case breq of
      Branches [_] -> return ()
      Branches [] | not missing -> return ()
      _ -> error' $ (if missing then "--current --missing needs one branch"
                     else "use --current with zero or one branches") ++
           " specified"
  if null pkgs
    then branchesPkg "."
    else mapM_ branchesPkg pkgs
  where
    branchesPkg :: FilePath -> IO ()
    branchesPkg path = do
      if mode == Remote
        then doBranchesPkg
        else
        withExistingDirectory path $
        if skipdead then
          unlessM (doesFileExist "dead.package")
          doBranchesPkg
        else doBranchesPkg
      where
        doBranchesPkg :: IO ()
        doBranchesPkg = do
          unless (mode == Remote) $
            unlessM isPkgGitRepo $
            error' "not Fedora dist-git"
          pkg <- getPackageName path
          if mode == Current
            then do
            br <- gitCurrentBranch'
            let onbranch =
                  case br of
                    RelBranch rbr -> Branches [rbr] == breq
                    OtherBranch _abr -> False
            if missing
              then unless onbranch $ putStrLn $ unPackage pkg ++ ": " ++ show br
              else case breq of
                     Branches [req] -> when (RelBranch req == br) $
                                       putStrLn $ unPackage pkg ++ ": " ++ show br
                     _ -> putStrLn $ unPackage pkg ++ ": " ++ show br
            else do
            brs <- delete "main" <$>
                   if mode == Remote
                   then pagurePkgBranches (unPackage pkg)
                   else localBranches False
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
                let havebrs = filter (`elem` branches) $ mapMaybe readBranch brs
                    result = if missing then branches \\ havebrs else havebrs
                unless (null result) $ do
                  putStr $ unPackage pkg ++ ": "
                  putStrLn $ (unwords . map show) result
