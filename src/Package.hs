module Package (
  clonePkg,
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  getChangeLog,
  getDirectoryName,
  getPackageName,
  findSpecfile,
  generateSrpm,
  buildRPMs,
  putPkgHdr,
  putPkgBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
  withBranchByPackages,
  withPackageByBranches,
  Package(..),
  packageSpec,
  pkgNameVerRel,
  pkgNameVerRel',
  rpmsNameVerRel,
  ConstrainBranches(..)
  ) where

import Common
import Common.System

import Distribution.Fedora
import SimpleCmd.Rpm

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

getDirectoryName :: IO String
getDirectoryName =
  takeFileName <$> getCurrentDirectory

getPackageName :: FilePath -> Package
getPackageName =
  Package . takeFileName

findSpecfile :: IO FilePath
findSpecfile = fileWithExtension ".spec"
  where
    -- looks in dir for a unique file with given extension
    fileWithExtension :: String -> IO FilePath
    fileWithExtension ext = do
      files <- filter (\ f -> takeExtension f == ext) <$> getDirectoryContents "."
      maybe (error' ("No unique " ++ ext ++ " file found")) return $ listToMaybe files

rpmEval :: String -> IO (Maybe String)
rpmEval s = do
  res <- cmd "rpm" ["--eval", s]
  return $ if null res || res == s then Nothing else Just res

generateSrpm :: Maybe Branch -> FilePath -> IO FilePath
generateSrpm mbr spec = do
  distopt <- case mbr of
               Nothing -> return []
               Just br -> do
                 dist <- branchDist br
                 return ["--define", "dist " ++ rpmDistTag dist]
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", "%{name}-%{version}-%{release}.src.rpm", spec]
  srcrpmdir <-
    ifM isPkgGitDir
      (return ".") $
      rpmEval "%{_srcrpmdir}" >>= maybe (error' "%_srcrpmdir undefined!") return
  let srpm = srcrpmdir </> srpmfile
      srpmdiropt = if null srcrpmdir then []
                   else ["--define", "_srcrpmdir " ++ srcrpmdir]
  ifM (notM $ doesFileExist srpm)
    (buildSrpm (distopt ++ srpmdiropt)) $
    do
    specTime <- getModificationTime spec
    srpmTime <- getModificationTime srpm
    if srpmTime > specTime
      then do
      -- pretty print with ~/
      putStrLn $ srpm ++ " is up to date"
      return srpm
      else buildSrpm (distopt ++ srpmdiropt)
  where
    buildSrpm opts = do
      srpm <- last . words <$> cmd "rpmbuild" (opts ++ ["-bs", spec])
      putStrLn $ "Created " ++ takeFileName srpm
      return srpm

buildRPMs :: Branch -> FilePath -> IO ()
buildRPMs br spec = do
  dist <- branchDist br
  cmd_ "rpmbuild" ["--define", "dist " ++ rpmDistTag dist, "-bb", spec]

putPkgHdr :: Package -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n= " ++ unpackage pkg ++ " ="

putPkgBrnchHdr :: Package -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ unpackage pkg ++ ":" ++ show br ++ " =="

withExistingDirectory :: FilePath -> IO () -> IO ()
withExistingDirectory dir act =
  ifM (doesDirectoryExist dir)
    (withCurrentDirectory dir act)
    (error' $ "No such directory: " ++ dir)

-- newly created Fedora repos/branches have just one README commit
initialPkgRepo :: IO Bool
initialPkgRepo = do
  commits <- length <$> gitShortLogN 2 Nothing
  return $ commits <= 1

data ConstrainBranches = LocalBranches | RemoteBranches | NoGitRepo
  deriving Eq

data Package = Package {unpackage :: String}

packagePath :: String -> (FilePath, Package)
packagePath path =
  (path, Package (takeFileName path))

packageSpec :: Package -> FilePath
packageSpec pkg = unpackage pkg <.> "spec"

-- do package over branches
withPackageByBranches :: ConstrainBranches -> (Package -> Branch -> IO ()) -> ([Branch],[String]) -> IO ()
withPackageByBranches constraint action (brs,pkgs) =
  if null pkgs
  then do
    gitdir <- isPkgGitDir
    when (not gitdir && constraint /= NoGitRepo) $
      error' "Not a pkg git dir"
    pkg <- Package <$> getDirectoryName
    withPackageDir constraint action brs (".", pkg)
  else mapM_ (withPackageDir constraint action brs) $ map packagePath pkgs

withPackageDir :: ConstrainBranches -> (Package -> Branch -> IO ()) -> [Branch] -> (FilePath, Package) -> IO ()
withPackageDir constraint action brs (dir, pkg) =
  withExistingDirectory dir $ do
  haveGit <- isPkgGitDir
  when (haveGit && constraint /= NoGitRepo) checkWorkingDirClean
  putPkgHdr pkg
  when haveGit $ git_ "fetch" []
  mcurrentbranch <- if haveGit then Just <$> gitCurrentBranch
                    else return Nothing
  branches <- if null brs then
                case constraint of
                  RemoteBranches -> fedoraBranches $ pagurePkgBranches (unpackage pkg)
                  LocalBranches -> fedoraBranches localBranches
                  NoGitRepo -> if haveGit then return $ maybeToList mcurrentbranch
                               else let singleton a = [a] in
                                      singleton <$> systemBranch
              else return brs
  when (null brs && constraint /= NoGitRepo) $
    putStrLn $ "Branches: " ++ unwords (map show branches) ++ "\n"
  mapM_ (action pkg) branches
  when (length brs /= 1) $
    whenJust mcurrentbranch gitSwitchBranch

-- do branch over packages
withBranchByPackages :: ConstrainBranches -> (Branch -> Package -> IO ()) -> ([Branch],[String]) -> IO ()
withBranchByPackages constraint action (brs,pkgs) = do
  when (null pkgs) $
    error' "Please give at least one package"
  when (null brs) $
    error' "Please specify at least one branch"
  forM_ brs $ \ br ->
    forM_ (map packagePath pkgs) $ \ (dir,pkg) ->
      withExistingDirectory dir $ do
      haveGit <- isPkgGitDir
      when (haveGit && constraint /= NoGitRepo) checkWorkingDirClean
      putPkgHdr pkg
      when haveGit $ git_ "fetch" []
      action br pkg

clonePkg :: Maybe Branch -> String -> IO ()
clonePkg mbr pkg =
  ifM (doesDirectoryExist pkg)
    {-then-} (putStrLn $ pkg ++ "/ already exists\n") $
    {-else-} do
      let mbranch = case mbr of
            Nothing -> []
            Just br -> ["--branch", show br]
      fasid <- fasIdFromKrb
      git_ "clone" $ ["--quiet"] ++ mbranch ++ ["ssh://" ++ fasid ++ "@pkgs.fedoraproject.org/rpms/" ++ pkg]

pkgNameVerRel :: Branch -> FilePath -> IO (Maybe String)
pkgNameVerRel br spec = do
  dist <- branchDist br
  listToMaybe <$> rpmspec ["--define", "dist " ++ rpmDistTag dist, "--srpm"] (Just "%{name}-%{version}-%{release}") spec

pkgNameVerRel' :: Branch -> FilePath -> IO String
pkgNameVerRel' br spec = do
  mnvr <- pkgNameVerRel br spec
  case mnvr of
    Nothing -> error' $ "rpmspec failed to parse " ++ spec
    Just nvr -> return nvr

rpmsNameVerRel :: Branch -> FilePath -> IO [String]
rpmsNameVerRel br spec = do
  dist <- branchDist br
  rpmspec ["--define", "dist " ++ rpmDistTag dist] (Just "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") spec

systemBranch :: IO Branch
systemBranch =
  readBranch' . init . removePrefix "PLATFORM_ID=\"platform:" <$> cmd "grep" ["PLATFORM_ID=", "/etc/os-release"]
