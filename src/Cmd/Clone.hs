module Cmd.Clone (cloneCmd, CloneRequest(..)) where

import Control.Monad (when)
import Fedora.Krb

import Branches
import Common.System
import qualified Common.Text as T
import Package
import Pagure

data CloneRequest = CloneGroup String
                  | CloneUser (Maybe String)
                  | ClonePkgs [String]

-- FIXME allow pagure repo wildcard
-- FIXME (detect commit rights or a ssh key?)
cloneCmd :: Maybe Branch -> CloneRequest -> IO ()
cloneCmd mbr request = do
  pkgs <- case request of
            CloneUser mid -> do
              userid <- maybe fasIdFromKrb return mid
              map (takeFileName . T.unpack) <$> pagureUserRepos srcfpo userid
            -- FIXME detect/prevent "path/dir"
            ClonePkgs ps -> return ps
            CloneGroup grp -> do
              map (takeFileName . T.unpack) <$> pagureGroupRepos srcfpo False grp
  mfas <- maybeFasIdFromKrb
  let no = length pkgs
  when (no > 1) $
    putStrLn $ "cloning" +-+ show no +-+ "pkg repos"
  let auth = maybe AnonClone (const UserClone) mfas
  mapM_ (clonePkg False auth mbr) pkgs
