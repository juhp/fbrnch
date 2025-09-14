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
      withExistingDirectory pkgdir $
      unlessM (doesFileExist "dead.package") $ do
      localbranches <- localBranches True
      oldcurrent <- gitCurrentBranch
      have1 <- haveBranch localbranches br1
      have2 <- haveBranch localbranches br2
      newcurrent <- gitCurrentBranch
      when (newcurrent /= oldcurrent) $
        gitSwitchBranch oldcurrent
      when (have1 && have2) $ do
        output <- ignoredLines <$> gitLines "log" (["--format=reference" | not long] ++ [show br1 ++ ".." ++ show br2])
        unless (null output) $ do
          unless (null pkgs) $
            getPackageName pkgdir >>= putPkgHdr
          mapM_ putStrLn output

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
