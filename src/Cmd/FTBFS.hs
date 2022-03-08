{-# LANGUAGE OverloadedStrings #-}

module Cmd.FTBFS (ftbfsCmd) where

import Branches
import Bugzilla
import Common
import Common.System
import qualified Common.Text as T
import Koji
import Package

-- FIXME option for status filter
-- FIXME check arch
ftbfsCmd :: Bool -> Bool -> Maybe String -> (Maybe Branch, [FilePath]) -> IO ()
ftbfsCmd dryrun getbugs muser (mbr,pkgs) = do
  session <- bzApiKeySession
  packages <-
    if null pkgs
    then
      if getbugs
      then do
        accountid <- getBzAccountId session muser
        map bugComponents <$>
          searchBugs session (query .&&. assigneeIs accountid)
      else return ["."]
    else if getbugs
         then error' $ "please use --bugs without listing a package"
         else return pkgs
  mapM_ (ftbfsPkg session) packages
  where
    bugComponents :: Bug -> String
    bugComponents bug =
      case bugComponent bug of
        [component] -> T.unpack component
        _ -> error' $ "multiple components!\n" ++ show bug
    query =
      ftbfsBugs .&&.
      case mbr of
        Nothing -> statusNewPost
        Just br -> statusNewPost .&&. versionIs (branchVersion br)

    ftbfsPkg :: BugzillaSession -> String -> IO ()
    ftbfsPkg session path = do
        pkg <- getPackageName path
        bugs <- searchBugs session $ pkgBugs (unPackage pkg) .&&. query
        unless (null bugs) $ do
          putStr $ (unPackage pkg) ++ ": "
          let br = fromMaybe Rawhide mbr
          exists <- doesDirectoryExist path
          if not exists
            then mapM_ putBug bugs
            else do
            nvr <- withExistingDirectory path $
                   localBranchSpecFile pkg (RelBranch br) >>=
                   -- FIXME handle %autorelease correctly here
                   pkgNameVerRel' br
            mstatus <- kojiBuildStatus nvr
            case mstatus of
              Nothing -> do
                putStrLn $ nvr ++ ": unknown status"
                mapM_ putBug bugs
              Just status -> do
                print status
                case status of
                  BuildFailed -> do
                    cmdLog "koji-tool" ["tasks", "-T", "-s", "fail", "-b", nvr]
                    putChar '\n'
                  BuildComplete -> do
                    case bugs of
                      [bug] ->
                        if bugStatus bug `elem` ["NEW", "ASSIGNED", "POST"]
                        then do
                          when dryrun $ putBug bug
                          putBugBuild dryrun session (bugId bug) nvr
                          putChar '\n'
                        else do
                          putBugURLStatus bug
                          putChar '\n'
                      _ -> mapM_ putBug bugs
                  _ -> return ()
