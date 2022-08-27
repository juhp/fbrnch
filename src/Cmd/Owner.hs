{-# LANGUAGE OverloadedStrings #-}

module Cmd.Owner (
  ownerCmd
  )
where

import Network.HTTP.Query (lookupKey)

import Common
import Common.System
import Package
import Pagure

-- FIXME option for email
-- FIXME option to output owner's packages on separate lines
ownerCmd :: [String] -> IO ()
ownerCmd pkgs = do
  if null pkgs
    then void $ ownerPkg "."
    else do
    pkgowners <- mapM ownerPkg pkgs
    when (length pkgs > 1) $ do
      putNewline
      let maintain = groupSort pkgowners
      forM_ maintain $ \(o,ps) ->
        putStrLn $ o ++ ": " ++ unwords (map unPackage ps)
  where
    ownerPkg :: String -> IO (String,Package)
    ownerPkg path = do
      pkg <- getPackageName path
      when (length pkgs > 1) $
        putStr $ unPackage pkg ++ " "
      epkginfo <- pagureProjectInfo srcfpo ("rpms" </> unPackage pkg)
      case epkginfo of
        Left err -> error' err
        Right pkginfo -> do
          case lookupKey "user" pkginfo of
            Nothing -> error' "user not found"
            Just user ->
              case userName user of
                Nothing -> error' "user name not found"
                Just (name,full) -> do
                  putStr $ name +-+ "(" ++ full ++ ")"
                  whenJust (lookupKey "access_users" pkginfo >>= lookupKey "admin") $ putStrLn . formatAdmins
                  return (name,pkg)

    userName user = do
      name <- lookupKey "name" user
      full <- lookupKey "fullname" user
      return (name,full)

    formatAdmins :: [String] -> String
    formatAdmins [] = ""
    formatAdmins as = ' ' : unwords as
