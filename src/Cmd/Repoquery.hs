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
  sysbr <- systemBranch
  mapM_ (repoquery_ sysbr query) brs
  where
    repoquery_ :: Branch -> [String] -> Branch -> IO ()
    repoquery_ sysbr query br =
      repoquery sysbr br query >>= putStrLn
