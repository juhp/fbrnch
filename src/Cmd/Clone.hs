module Cmd.Clone (cloneCmd) where

import Package
import Types

cloneCmd :: Maybe Branch -> [Package] -> IO ()
cloneCmd mbr =
  mapM_ clonePkg
  where
    clonePkg :: Package -> IO ()
    clonePkg pkg = do
      let mbranch = case mbr of
            Nothing -> []
            Just br -> ["--branch", show br]
      fedpkg_ "clone" $ mbranch ++ [pkg]
