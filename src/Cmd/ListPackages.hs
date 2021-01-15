{-# LANGUAGE OverloadedStrings #-}

module Cmd.ListPackages (
  listCmd,
  Packager(..)
  )
where

import Data.Aeson
import Fedora.Pagure

import Common
import Common.System
import qualified Common.Text as T
import Pagure

data Packager = Owner String | Committer String

-- FIXME remote/pagures branch and --remote or --no-remote
listCmd :: Bool -> Maybe Packager -> [String] -> IO ()
listCmd count mpackager pkgs = do
  unless (count || isJust mpackager || not (null pkgs)) $
    error' "Please give a package pattern, --count, or --owner/--username"
  if null pkgs then listPackage Nothing
    else mapM_ (listPackage . Just) pkgs
  where
    listPackage :: Maybe String -> IO ()
    listPackage mpattern = do
      let path = "projects"
          params = makeKey "short" "1" ++ fork ++ packager ++ makeKey "namespace" "rpms" ++ maybeKey "pattern" mpattern
      pages <- queryPaged srcfpo count path params ("pagination", "page")
      mapM_ (printPage . toObject) pages
      where
        packager =
          case mpackager of
            Nothing -> makeKey "owner" "!orphan"
            Just (Owner o) -> makeKey "owner" o
            Just (Committer c) -> makeKey "username" c

        fork = makeKey "fork" "0"

        printPage :: Object -> IO ()
        printPage result =
          let projects = lookupKey' "projects" result :: [Object]
          in
          mapM_ (T.putStrLn . lookupKey' "name") projects

        toObject :: Value -> Object
        toObject (Object obj) = obj
        toObject v = error $ "not object: " ++ show v

-- FIXME limit max number of pages (10?) or --pages
queryPaged :: String -> Bool -> String -> Query -> (String,String) -> IO [Value]
queryPaged server count path params (pagination,paging) =
  if count
    then do
    mnum <- queryPagureCount server path params pagination
    print $ fromMaybe (error' "pages not found") mnum
    return []
    else
    queryPagurePaged server path params (pagination,paging)
