{-# LANGUAGE OverloadedStrings #-}

module Cmd.FTBFS (
  ftbfsCmd,
  FTBFSBugs(..)
  )
where

import Branches
import Bugzilla
import Common
import Common.System
import qualified Common.Text as T
import Koji
import Package

data FTBFSBugs = FtbfsUser (Maybe String) | FtbfsSubstring String

-- FIXME option for status filter
-- FIXME check arch
-- FIXME ask resolution
ftbfsCmd :: Bool -> Bool -> Maybe FTBFSBugs -> (Maybe Branch, [FilePath])
         -> IO ()
ftbfsCmd dryrun short mbugsopt (mbr,pkgs) = do
  case mbugsopt of
    Just bugsopt -> do
      unless (null pkgs) $
        error' $ "Cannot combine bugs option with" +-+ pluralOnly pkgs "package"
      session <- bzApiKeySession
      bugs <-
        case bugsopt of
          FtbfsUser muser -> do
            accountid <- getBzAccountId session muser
            searchBugs session (query .&&. assigneeIs accountid)
          FtbfsSubstring substr ->
            searchBugs session (query .&&. componentSubStr substr)
      mapM_ (ftbfsBugs session) bugs
    Nothing ->
      mapM_ ftbfsPkg $ if null pkgs then ["."] else pkgs
  where
    bugComponents :: Bug -> String
    bugComponents bug =
      case bugComponent bug of
        [component] -> T.unpack component
        _ -> error' $ "multiple components!\n" ++ show bug

    query =
      ftbfsFedoraBugs .&&.
      case mbr of
        Nothing -> statusNewModified
        Just br -> statusNewModified .&&. versionIs (branchVersion br)

    ftbfsBugs :: BugzillaSession -> Bug -> IO ()
    ftbfsBugs session bug = do
      let pkg = bugComponents bug
      handleBug session pkg (Package pkg) bug

    ftbfsPkg :: FilePath -> IO ()
    ftbfsPkg path = do
      pkg <- getPackageName path
      session <- bzApiKeySession
      mbug <- bzBugMaybe session $ pkgBugs (unPackage pkg) .&&. query
      whenJust mbug $ handleBug session path pkg

    handleBug :: BugzillaSession -> FilePath -> Package -> Bug -> IO ()
    handleBug session path pkg bug = do
      if short then
        putStrLn $ unPackage pkg
        else do
        putStr $ unPackage pkg ++ ": "
        let br = fromMaybe Rawhide mbr
        exists <- doesDirectoryExist path
        if not exists
          then putBug bug
          else do
          nvr <- withExistingDirectory path $
                 localBranchSpecFile pkg (RelBranch br) >>=
                 -- FIXME handle %autorelease correctly here
                 pkgNameVerRel' br
          mstatus <- kojiBuildStatus nvr
          case mstatus of
            Nothing -> do
              putStrLn $ showNVR nvr ++ ": unknown nvr"
              putBug bug
            Just status -> do
              print status
              case status of
                BuildFailed -> do
                  cmdLog "koji-tool" ["tasks", "--details", "-T", "-s", "fail", "-b", showNVR nvr]
                  putNewLn
                BuildComplete -> do
                  if bugStatus bug `elem` ["NEW", "ASSIGNED", "POST"]
                  then do
                    when dryrun $ putBug bug
                    putBugBuild dryrun session (bugId bug) nvr
                    putNewLn
                  else do
                    putBugURLStatus bug
                    putNewLn
                _ -> return ()
