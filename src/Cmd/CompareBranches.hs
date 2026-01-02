module Cmd.CompareBranches (
  compareBranchesCmd)
where

import Branches
import Common
import Common.System
import Git
import Package

-- FIXME warn if older branch ahead
compareBranchesCmd :: Bool -> Maybe String -> AnyBranch -> AnyBranch -> [String] -> IO ()
compareBranchesCmd long mignore br1 br2 pkgs = do
  if null pkgs
    then do
    unlessM isPkgGitRepo $
      error' "Please specify at least one package"
    else do
    whenM isPkgGitRepo $
      error' "Cannot specify multiple packages inside a package dir"
  let packages = if null pkgs then ["."] else pkgs
  mapM_ comparePkg packages
  where
    comparePkg :: String -> IO ()
    comparePkg pkgdir =
      withExistingDirectory pkgdir $ do
      local <- compareLocal
      remote <- compareRemote
      unless (null local && null remote) $ do
        unless (null pkgs) $
          getPackageName pkgdir >>= putPkgHdr
        unless (null local) $ do
          putStrLn "local changes:"
          mapM_ putStrLn local
        unless (null remote) $ do
          putStrLn "remote changes:"
          mapM_ putStrLn remote

    logFormat = "--pretty=format:%h %s (%cs)"

    compareLocal :: IO [String]
    compareLocal = do
      localbranches <- localBranches True
      oldcurrent <- gitCurrentBranch
      have1 <- haveBranch localbranches br1
      have2 <- haveBranch localbranches br2
      newcurrent <- gitCurrentBranch
      when (newcurrent /= oldcurrent) $
        gitSwitchBranch oldcurrent
      if have1 && have2
        then ignoredLines <$> gitLines "log" ([logFormat | not long] ++ [show br1 ++ ".." ++ show br2])
        else do
        let missing =
              map show $ [br1 | not have1] ++ [br2 | not have2]
        warning $ "didn't find local" +-+ unwords missing
        return []

    compareRemote :: IO [String]
    compareRemote = do
      have1 <- checkIfRemoteBranchExists br1
      have2 <- checkIfRemoteBranchExists br2
      if have1 && have2
        then ignoredLines <$> gitLines "log" ([logFormat | not long] ++ ["origin/" ++ show br1 ++ ".." ++ "origin/" ++ show br2])
        else do
        let missing =
              map show $ [br1 | not have1] ++ [br2 | not have2]
        warning $ "didn't find remote" +-+ unwords missing
        return []

    ignoredLines :: [String] -> [String]
    ignoredLines =
      case mignore of
        Nothing -> id
        Just ignore -> filter (not . (ignore `isInfixOf`))

    haveBranch :: [String] -> AnyBranch -> IO Bool
    haveBranch locals br =
      if show br `elem` locals
      then return True
      else gitSwitchBranch' True $ onlyRelBranch br
