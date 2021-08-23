module Cmd.Repoquery
  (repoqueryCmd)
where

import Branches
import Common.System
import Package

import qualified Data.List as L

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
    repoquery_ sysbr query br = do
      let qf = if null (filter ("--qf" `L.isPrefixOf`) query)
               then ["--queryformat=%{repoid}: %{name}-%{version}-%{release}.%{arch}"]
               else []
      repoquery sysbr br (qf ++ query) >>= putStrLn
