module Cmd.PRs (
  prsCmd
  )
where

import Common
import Common.System
import Pagure

prsCmd :: [String] -> IO ()
prsCmd pkgs = do
  packages <-
    if null pkgs then singleton <$> getDirectoryName else return pkgs
  mapM_ pagurePRs packages
