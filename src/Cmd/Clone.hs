module Cmd.Clone (cloneCmd, CloneRequest(..)) where

import Common.System
import qualified Common.Text as T

import Branches
import Krb
import Package
import Pagure

data CloneRequest = CloneUser (Maybe String)
                  | ClonePkgs (Maybe Branch, [String])

-- FIXME allow pagure repo wildcard
-- FIXME (detect commit rights or a ssh key?)
cloneCmd :: CloneRequest -> IO ()
cloneCmd request = do
  (mbr,pkgs) <- case request of
            CloneUser mid -> do
              userid <- maybe fasIdFromKrb return mid
              ps <- map (takeFileName . T.unpack) <$> pagureUserRepos srcfpo userid
              return (Nothing, ps)
            -- FIXME detect/prevent "path/dir"
            ClonePkgs mbps -> return mbps
  mfas <- maybeFasIdFromKrb
  let auth = maybe AnonClone (const UserClone) mfas
  mapM_ (clonePkg False auth mbr) pkgs
