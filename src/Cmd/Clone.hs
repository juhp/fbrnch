module Cmd.Clone (cloneCmd, CloneRequest(..)) where

import Common.System
import qualified Common.Text as T

import Branches
import Krb
import Package
import Pagure

data CloneRequest = CloneUser (Maybe String) | ClonePkgs [String]

-- FIXME allow pagure repo wildcard
-- FIXME --anonymous (detect commit rights? or a ssh key?)
cloneCmd :: Maybe Branch -> CloneRequest -> IO ()
cloneCmd mbr request = do
  pkgs <- case request of
            CloneUser mid -> do
              fasid <- maybe fasIdFromKrb return mid
              map (takeFileName . T.unpack) <$> pagureUserRepos srcfpo fasid
            ClonePkgs ps -> return ps
  mapM_ clonePkg pkgs
  where
    clonePkg :: Package -> IO ()
    clonePkg pkg = do
      exists <- doesDirectoryExist pkg
      if exists then
        putStrLn $ pkg ++ "/ already exists\n"
        else do
        let mbranch = case mbr of
              Nothing -> []
              Just br -> ["--branch", show br]
        fedpkg_ "clone" $ mbranch ++ [pkg]
