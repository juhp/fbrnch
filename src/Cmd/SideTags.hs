module Cmd.SideTags (sideTagsCmd) where

import Branches
import Koji

sideTagsCmd :: [Branch] -> IO ()
sideTagsCmd brs = do
  branches <-
    if null brs
    then getFedoraBranches
    else return brs
  mapM_ sideTagsBranch branches
  where
    sideTagsBranch :: Branch -> IO ()
    sideTagsBranch br =
      kojiUserSideTags br >>= mapM_ putStrLn
