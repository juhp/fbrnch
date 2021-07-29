module Cmd.Repoquery
  (repoqueryCmd)
where

import Branches
import Common.System
import Package

repoqueryCmd :: (BranchesReq, [String]) -> IO ()
repoqueryCmd (breq, pkgs) = do
  query <- if null pkgs
           then pure <$> getDirectoryName
           else return pkgs
  brs <- listOfBranches True False breq
  mapM_ (repoquery_ query) brs
  where
    repoquery_ :: [String] -> Branch -> IO ()
    repoquery_ query br =
      repoquery br query >>= putStrLn
