module Cmd.Bugs (bugsCmd) where

import Bugzilla
import Package

bugsCmd :: Maybe String -> [String] -> IO ()
bugsCmd keyword pkgs = do
  if null pkgs
    then bugsPkg "."
    else mapM_ bugsPkg pkgs
  where
    bugsPkg :: String -> IO ()
    bugsPkg path = do
      pkg <- getPackageName path
      putPkgHdr pkg
      let query =
            case keyword of
              Nothing -> statusOpen
              Just key -> statusOpen .&&. summaryContains key
      (bugs, _) <- bugsSession $ pkgBugs (unPackage pkg) .&&. query
      mapM_ putBugVer $ sortBugsByProduct bugs
