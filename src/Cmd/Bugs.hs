module Cmd.Bugs (bugsCmd) where

import Bugzilla
import Package

bugsCmd :: Maybe String -> IO ()
bugsCmd mpkg = do
  pkg <- maybe getDirectoryName return mpkg
  (bugs, _) <- bugsSession $ pkgBugs pkg .&&. statusOpen
  mapM_ putBugVer $ sortBugsByProduct bugs
