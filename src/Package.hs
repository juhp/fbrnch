module Package (
  clonePkg,
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  getChangeLog,
  getPackageName,
  findSpecfile,
  generateSrpm,
  getSpecFile,
  putPkgHdr,
  putPkgBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
  withPackageBranches,
  withPackageDir,
  Package
  ) where

import Common
import Common.System

import Branches
import Git
import Krb
import Prompt

fedpkg :: String -> [String] -> IO String
fedpkg c args =
  cmd "fedpkg" (c:args)

fedpkg_ :: String -> [String] -> IO ()
fedpkg_ c args =
  cmd_ "fedpkg" (c:args)

checkForSpecFile :: String -> IO ()
checkForSpecFile spec = do
  have <- doesFileExist spec
  unless have $ error' $ spec ++ " not found"

getChangeLog :: FilePath -> IO String
getChangeLog spec = do
  clog <- cleanChangelog <$> cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
  putStrLn clog
  usrlog <- prompt "Press Enter to use above or input update summary now"
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
  putStrLn $ "\n= " ++ takeFileName pkg ++ " ="

putPkgBrnchHdr :: String -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ takeFileName pkg ++ ":" ++ show br ++ " =="

withExistingDirectory :: FilePath -> IO () -> IO ()
withExistingDirectory dir act = do
  hasDir <- doesDirectoryExist dir
  if not hasDir
    then error' $ "No such directory: " ++ dir
    else
    withCurrentDirectory dir act

-- newly created Fedora repos/branches have just one README commit
initialPkgRepo :: IO Bool
initialPkgRepo = do
  commits <- length <$> gitShortLogN 2 Nothing
  return $ commits <= 1

type Package = String

withPackageBranches :: Bool -> (Maybe Package -> Branch -> IO ()) -> ([Branch],[Package]) -> IO ()
withPackageBranches write action (brs,pkgs) =
  if null pkgs
  then do
    checkIsPkgGitDir
    currentbranch <- getCurrentBranch
    branches <- if null brs then packageBranches else return $ (reverse . sort) brs
    mapM_ (action Nothing) branches
    unless (length brs == 1) $
      gitSwitchBranch currentbranch
  else mapM_ (withPackageDir write action brs) pkgs

withPackageDir :: Bool -> (Maybe Package -> Branch -> IO ()) -> [Branch] -> Package -> IO ()
withPackageDir write action brs pkg =
  withExistingDirectory pkg $ do
    when write checkWorkingDirClean
    putPkgHdr pkg
    git_ "fetch" []
    branches <- if null brs then packageBranches else return $ (reverse . sort) brs
    when (null brs && write) $
      putStrLn $ "\nBranches: " ++ unwords (map show branches)
    currentbranch <- getCurrentBranch
    mapM_ (action (Just pkg)) branches
    unless (length brs == 1) $
      gitSwitchBranch currentbranch

clonePkg :: Maybe Branch -> Package -> IO ()
clonePkg mbr pkg = do
  exists <- doesDirectoryExist pkg
  if exists then
    putStrLn $ pkg ++ "/ already exists\n"
    else do
    let mbranch = case mbr of
          Nothing -> []
          Just br -> ["--branch", show br]
    fasid <- fasIdFromKrb
    git_ "clone" $ mbranch ++ ["ssh://" ++ fasid ++ "@pkgs.fedoraproject.org/rpms/" ++ pkg]
