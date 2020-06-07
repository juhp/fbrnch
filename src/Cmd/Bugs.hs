module Cmd.Bugs (bugsCmd) where

import Bugzilla
import Package

bugsCmd :: Maybe String -> IO ()
bugsCmd mpkg = do
  pkg <- case mpkg of
    Nothing -> getDirectoryName
    Just pkg -> return pkg
  (bugs, _) <- bugsSession $ pkgBugs pkg .&&. statusOpen
  mapM_ putBug $ sortBugsByProduct bugs
