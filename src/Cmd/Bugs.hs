module Cmd.Bugs (bugsCmd) where

import Bugzilla
import Package

bugsCmd :: [String] -> IO ()
bugsCmd pkgs = do
  if null pkgs
    then bugsPkg "."
    else mapM_ bugsPkg pkgs
  where
    bugsPkg :: String -> IO ()
    bugsPkg path = do
      pkg <- getPackageName path
      putPkgHdr pkg
      (bugs, _) <- bugsSession $ pkgBugs (unPackage pkg) .&&. statusOpen
      mapM_ putBugVer $ sortBugsByProduct bugs
