module Cmd.Repoquery
  (repoqueryCmd)
where

import Branches
import Common
import Common.System
import Package

repoqueryCmd :: (BranchesReq, [String]) -> IO ()
repoqueryCmd (breq, pkgs) = do
  when (breq == Branches []) $
    error' "no branch specified"
  query <- if null pkgs
           then pure <$> getDirectoryName
           else return pkgs
  brs <- listOfBranches True False breq
  mapM_ (repoquery_ query) brs
  where
    repoquery_ :: [String] -> Branch -> IO ()
    repoquery_ query br =
      repoquery br query >>= putStrLn
