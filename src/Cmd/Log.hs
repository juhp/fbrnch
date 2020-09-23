module Cmd.Log (
  logCmd)
where

import Branches
import Common
import Common.System
import Git
import Package

logCmd :: Bool -> AnyBranch -> AnyBranch -> [String] -> IO ()
logCmd long br1 br2 pkgs = do
  if null pkgs
    then do
      unlessM isPkgGitRepo $
        error' "Please specify at least one package"
    else do
      whenM isPkgGitRepo $
        error' "Cannot specify multiple packages inside a package dir"
  let packages = if null pkgs then ["."] else pkgs
  mapM_ logPkg packages
  where
    logPkg :: String -> IO ()
    logPkg pkgdir =
      withExistingDirectory pkgdir $ do
      unless (null pkgs) $
        getPackageName pkgdir >>= putPkgHdr
      let (br1',br2') =
            case (br1, br2) of
              (RelBranch b1, RelBranch b2) | b2 < b1 -> (br2,br1)
              _ -> (br1,br2)
        in
        git_ "log" $ ["--format=reference" | not long] ++ [show br1' ++ ".." ++ show br2']
