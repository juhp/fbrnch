{-# LANGUAGE CPP #-}

module Cmd.PRs (
  prsCmd
  )
where

#if MIN_VERSION_base(4,15,0)
import Common (singleton)
#endif
import Common.System (getDirectoryName)
import Pagure (pagurePRs)

prsCmd :: [String] -> IO ()
prsCmd pkgs = do
  packages <-
    if null pkgs then singleton <$> getDirectoryName else return pkgs
  mapM_ pagurePRs packages
#if !MIN_VERSION_base(4,15,0)
  where
    singleton x = [x]
#endif
