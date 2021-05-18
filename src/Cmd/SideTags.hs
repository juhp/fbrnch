module Cmd.SideTags (sideTagsCmd) where

import Branches
import Koji

sideTagsCmd :: [Branch] -> IO ()
sideTagsCmd brs = do
  if null brs
    then kojiUserSideTags Nothing >>= mapM_ putStrLn
    else mapM_ sideTagsBranch brs
  where
    sideTagsBranch :: Branch -> IO ()
    sideTagsBranch br =
      kojiUserSideTags (Just br) >>= mapM_ putStrLn
