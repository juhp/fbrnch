module Cmd.Repoquery
  (repoqueryCmd)
where

import Branches
import Common.System

repoqueryCmd :: (BranchesReq, [String]) -> IO ()
repoqueryCmd (breq, pkgs) = do
  query <- if null pkgs
           then pure <$> getDirectoryName
           else return pkgs
  brs <- listOfBranches True False breq
  cmd_ "fedora-repoquery" $ ["--qf", "nvr"] ++ map showBranch brs ++ query
