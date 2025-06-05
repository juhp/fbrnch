{-# LANGUAGE OverloadedStrings #-}

module Cmd.Clone (cloneCmd, CloneRequest(..)) where

import Control.Monad.Extra (concatMapM, when)
import Fedora.Krb

import Branches
import Cmd.ListPackages (listPackages)
import Common.System
import qualified Common.Text as T
import Package
import Pagure

data CloneRequest = CloneGroup String
                  | CloneUser (Maybe String)
                  | ClonePkgs [String]

-- FIXME --exclude
-- FIXME (detect commit rights or a ssh key?)
cloneCmd :: Bool -> Maybe Branch -> CloneRequest -> IO ()
cloneCmd dryrun mbr request = do
  pkgs <- case request of
            CloneUser mid -> do
              userid <- maybe fasIdFromKrb return mid
              map (takeFileName . T.unpack) <$> pagureUserRepos srcfpo userid
            -- FIXME detect/prevent "path/dir"
            ClonePkgs ps -> concatMapM globPkgs ps
            CloneGroup grp -> do
              map (takeFileName . T.unpack) <$> pagureGroupRepos srcfpo False grp
  mfas <- maybeFasIdFromKrb
  let no = length pkgs
  when (no > 1) $
    putStrLn $ "cloning" +-+ show no +-+ "pkg repos"
  let auth = maybe AnonClone (const UserClone) mfas
  mapM_ (clonePkg dryrun False auth mbr) pkgs

-- FIXME force
globPkgs :: String -> IO [String]
globPkgs pat =
  if '*' `elem` pat
  then listPackages False Nothing [pat]
  else return [pat]
