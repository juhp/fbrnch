module Package where

import Control.Monad
import Data.Maybe
import Distribution.Fedora.Branch
import SimpleCmd
import System.Directory
import System.FilePath

import Git
import Prompt

fedpkg :: String -> [String] -> IO String
fedpkg c args =
  cmd "fedpkg" (c:args)

fedpkg_ :: String -> [String] -> IO ()
fedpkg_ c args =
  cmd_ "fedpkg" (c:args)

-- newly created Fedora repos/branches have just one README commit
initialPkgRepo :: IO Bool
initialPkgRepo = do
  commits <- length <$> gitShortLogN 2 Nothing
  return $ commits <= 1

checkForSpecFile :: String -> IO ()
checkForSpecFile pkg = do
  let spec = pkg <.> "spec"
  have <- doesFileExist $ pkg <.> "spec"
  unless have $ error' $ spec ++ " not found"

getChangeLog :: FilePath -> IO String
getChangeLog spec = do
  clog <- cleanChangelog <$> cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
  putStrLn clog
  usrlog <- prompt "to use above or input update summary now"
  return $ if null usrlog then clog else usrlog
  where
    cleanChangelog cs =
      case length (lines cs) of
        0 -> error' "empty changelog" -- should not happen
        1 -> removePrefix "- " cs
        _ -> cs

getPackageName :: Maybe FilePath -> IO String
getPackageName mdir =
  takeFileName <$> maybe getCurrentDirectory return mdir

findSpecfile :: IO FilePath
findSpecfile = fileWithExtension ".spec"
  where
    -- looks in dir for a unique file with given extension
    fileWithExtension :: String -> IO FilePath
    fileWithExtension ext = do
      files <- filter (\ f -> takeExtension f == ext) <$> getDirectoryContents "."
      maybe (error' ("No unique " ++ ext ++ " file found")) return $ listToMaybe files

-- FIXME assumed srpm in local dir
generateSrpm :: FilePath -> IO FilePath
generateSrpm spec = do
  nvr <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}-%{version}-%{release}", spec]
  let srpm = nvr <.> "src.rpm"
  haveSrpm <- doesFileExist srpm
  if haveSrpm then do
    specTime <- getModificationTime spec
    srpmTime <- getModificationTime srpm
    if srpmTime > specTime
      then do
      putStrLn $ srpm ++ " is up to date"
      return srpm
      else buildSrpm
    else buildSrpm
  where
    buildSrpm = do
      srpm <- takeFileName . last . words <$> cmd "rpmbuild" ["-bs", spec]
      putStrLn $ "Created " ++ srpm
      return srpm

getSpecFile :: Maybe FilePath -> IO String
getSpecFile =
  -- FIXME or change to dir
  maybe findSpecfile checkLocalFile
  where
    checkLocalFile :: FilePath -> IO FilePath
    checkLocalFile f =
      if takeFileName f == f then return f
        else error' "Please run in the directory of the spec file"

putPkgHdr :: String -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n= " ++ pkg ++ " ="

putPkgBrnchHdr :: String -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ pkg ++ ":" ++ show br ++ " =="

withExistingDirectory :: FilePath -> IO () -> IO ()
withExistingDirectory dir act = do
  hasDir <- doesDirectoryExist dir
  if not hasDir
    then error' $ "No such directory: " ++ dir
    else
    withCurrentDirectory dir act
