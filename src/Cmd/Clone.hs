module Cmd.Clone (cloneCmd, CloneRequest(..)) where

import Common.System
import qualified Common.Text as T

import Branches
import Krb
import Package
import Pagure

data CloneRequest = CloneUser (Maybe String)
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
  mfas <- maybeFasIdFromKrb
  let auth = maybe AnonClone (const UserClone) mfas
  mapM_ (clonePkg False auth mbr) pkgs
