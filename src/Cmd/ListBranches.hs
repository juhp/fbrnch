module Cmd.ListBranches (
  branchesCmd,
  BranchesMode(..),
  BranchDead(..)
  )
where

import Distribution.Fedora.Branch (getActiveBranches, readBranch)

import Branches
import Common
import Common.System
import Git
import Package

data BranchesMode = Local | Remote | Current
  deriving Eq

data BranchDead = SkipDead | OnlyDead

-- FIXME remote/pagures branch and --remote or --no-remote
-- FIXME --local for existing local branches
branchesCmd :: Maybe BranchDead -> Bool -> Bool -> BranchesMode
            -> (BranchesReq,[String]) -> IO ()
branchesCmd mdead allbrs missing mode (breq, pkgs) = do
  -- when (allbrs $ do
  --   unless (null brs) $
  --     error' "cannot combine --all and branches"
  --   when missing $
  --     error' "cannot combine --all and --missing"
  when (mode == Current) $
    case breq of
      Branches [_] -> return ()
      Branches [] | not missing -> return ()
      _ -> error' $ (if missing
                     then "--current --missing needs one branch"
                     else "use --current with zero or one branches") +-+
           "specified"
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
        case mdead of
          Just SkipDead ->
            unlessM (doesFileExist "dead.package")
            doBranchesPkg
          Just OnlyDead ->
            whenM (doesFileExist "dead.package")
            doBranchesPkg
          Nothing -> doBranchesPkg
      where
        doBranchesPkg :: IO ()
        doBranchesPkg = do
          unless (mode == Remote) $
            unlessM isPkgGitRepo $
            error' "not Fedora dist-git"
          pkg <- getPackageName path
          let pkgprefix =
                if length pkgs > 1
                then unPackage pkg ++ ":"
                else ""
          if mode == Current
            then do
            br <- gitCurrentBranch'
            let onbranch =
                  case br of
                    RelBranch rbr -> Branches [rbr] == breq
                    OtherBranch _abr -> False
            if missing
              then unless onbranch $ putStrLn $ pkgprefix +-+ show br
              else case breq of
                     Branches [req] -> when (RelBranch req == br) $
                                       putStrLn $ pkgprefix +-+ show br
                     _ -> putStrLn $ pkgprefix +-+ show br
            else do
            brs <- delete "main" <$>
                   if mode == Remote
                   then pagurePkgBranches (unPackage pkg)
                   else localBranches False
            if allbrs then do
              -- FIXME epel branches are not sorted correctly: epel10 epel10.0 epel9 f41 ...
              putStrLn $ pkgprefix +-+ unwords brs
              else do
              if breq == Branches []
                then do
                -- FIXME better to filter inactive instead
                active <- getActiveBranches
                let result =
                      if missing
                      then active \\ mapMaybe readBranch brs
                      else activeBranches active brs
                putStrLn $ pkgprefix +-+ (unwords . map showBranch) result
                else do
                branches <- listOfBranches True False breq
                let havebrs = filter (`elem` branches) $ mapMaybe readBranch brs
                    result = if missing then branches \\ havebrs else havebrs
                unless (null result) $
                  putStrLn $ pkgprefix +-+ (unwords . map showBranch) result
