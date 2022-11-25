{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cmd.Bugs
  (bugsCmd,
   bzusersCmd)
where

import Data.List.Extra (trim)

import Bugzilla
import Common.System (error')
import qualified Common.Text as T
import Package

bugsCmd :: Maybe String -> [String] -> IO ()
bugsCmd keyword pkgs = do
  if null pkgs
    -- FIXME check for distgit
    then bugsPkg "."
    else mapM_ bugsPkg pkgs
  where
    bugsPkg :: String -> IO ()
    bugsPkg path = do
      pkg <- getPackageName path
      putPkgHdr pkg
      let query =
            case keyword of
              Nothing -> statusOpen
              Just key -> statusOpen .&&. summaryContains key
      bugs <- bugsAnon $ pkgBugs (unPackage pkg) .&&. query
      mapM_ putBugVer $ sortBugsByProduct bugs

bzusersCmd :: String -> IO ()
bzusersCmd name = do
  if length (trim name) < 3
    then error' "use more than 2 characters"
    else do
    session <- bzApiKeySession
    users <- searchUsers session (T.pack name)
    mapM_ printUser users
  where
    printUser :: User -> IO ()
    printUser User{..} =
      T.putStrLn $ userRealName <> " " <> "<" <> userName <> ">"
