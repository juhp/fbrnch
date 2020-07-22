module Package (
  builtRpms,
  clonePkg,
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  getChangeLog,
  getDirectoryName,
  getPackageName,
  findSpecfile,
  localBranchSpecFile,
  generateSrpm,
  buildRPMs,
  prepPackage,
  getSources,
  putPkgHdr,
  putPkgBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
--  withBranchByPackages,
  withPackageByBranches,
  Package(..),
  packageSpec,
  pkgNameVerRel,
  pkgNameVerRel',
  buildRequires,
  notInstalled,
  pkgInstalled,
  systemBranch
  ) where

import Common
import Common.System

import Distribution.Fedora
import SimpleCmd.Rpm

import Branches
import Git
import InterleaveOutput
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
      files <- filter ((== ext) . takeExtension) <$> listDirectory "."
      maybe (error' ("No unique " ++ ext ++ " file found")) return $ listToMaybe files

localBranchSpecFile :: Package -> Branch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitDir
  when gitdir $ do
    putPkgBrnchHdr pkg br
    gitSwitchBranch br
  if gitdir
    then return $ packageSpec pkg
    else findSpecfile

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

-- FIXME pull sources
buildRPMs :: Bool -> Bool -> Branch -> FilePath -> IO ()
buildRPMs quiet shortcircuit br spec = do
  dist <- branchDist br
  cwd <- getCurrentDirectory
  gitDir <- isGitDir "."
  let buildopt = if shortcircuit then ["-bi", "--short-circuit"] else ["-bb"]
      rpmdirs =
        [ "--define="++ mcr +-+ cwd | gitDir,
          mcr <- ["_builddir", "_rpmdir", "_srcrpmdir", "_sourcedir"]]
      args = rpmdirs ++ ["--define", "dist " ++ rpmDistTag dist] ++ buildopt ++ [spec]
  if not quiet then
    cmd_ "rpmbuild" args
    else do
    putStr "Building locally: "
    cmdSilent_ "rpmbuild" args
    putStrLn "done"

prepPackage :: Package -> Branch -> IO ()
prepPackage pkg br = do
  ifM (doesFileExist "dead.package")
    (putStrLn "dead.package") $
    do
    spec <- localBranchSpecFile pkg br
    unlessM (doesFileExist spec) $
      error' $ spec ++ " not found"
    gitDir <- getSources spec
    cwd <- getCurrentDirectory
    let rpmdirs =
          [ "--define="++ mcr +-+ cwd | gitDir,
            mcr <- ["_builddir", "_sourcedir"]]
        args = rpmdirs ++ ["-bp", spec]
    nvr <- pkgNameVerRel' br spec
    putStr $ "Prepping " ++ nvr ++ ": "
    cmdSilent_ "rpmbuild" args
    putStrLn "done"

getSources :: FilePath -> IO Bool
getSources spec = do
    gitDir <- isGitDir "."
    srcdir <- getSourceDir gitDir
    srcs <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
    unless gitDir $
      unlessM (doesDirectoryExist srcdir) $
      createDirectoryIfMissing True srcdir
    forM_ srcs $ \ src -> do
      unlessM (doesFileExist (srcdir </> src)) $ do
        uploaded <-
          if gitDir then do
            have_sources <- doesFileExist "sources"
            if have_sources then
              grep_ src "sources"
              else return False
          else return False
        if uploaded
          then cmd_ "fedpkg" ["sources"]
          else cmd_ "spectool" ["-g", "-S", "-C", srcdir, spec]
    return gitDir
  where
    sourceFieldFile :: String -> FilePath
    sourceFieldFile field =
      if null field then
        -- should be impossible
        error "empty source field!"
      else (takeFileName . last . words) field

    getSourceDir :: Bool -> IO FilePath
    getSourceDir gitDir = do
      if gitDir
        then getCurrentDirectory
        else fromJust <$> rpmEval "%{_sourcedir}"

withExistingDirectory :: FilePath -> IO a -> IO a
withExistingDirectory dir act =
  ifM (doesDirectoryExist dir)
    (withCurrentDirectory dir act)
    (error' $ "No such directory: " ++ dir)

-- newly created Fedora repos/branches have just one README commit
initialPkgRepo :: IO Bool
initialPkgRepo = do
  commits <- length <$> gitShortLogN 2 Nothing
  return $ commits <= 1

newtype Package = Package {unPackage :: String}

putPkgHdr :: Package -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n= " ++ unPackage pkg ++ " ="

putPkgBrnchHdr :: Package -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ unPackage pkg ++ " " ++ show br ++ " =="

packagePath :: String -> (FilePath, Package)
packagePath path =
  (path, Package (takeFileName path))

packageSpec :: Package -> FilePath
packageSpec pkg = unPackage pkg <.> "spec"

-- do package over branches
withPackageByBranches :: Bool -> Bool -> Bool -> (Package -> Branch -> IO ()) -> (Branches,[String]) -> IO ()
withPackageByBranches quiet clean needDistgit action (brnchs,pkgs) = do
  if null pkgs
    then do
    when needDistgit $ do
      unlessM isPkgGitDir $
        error' "Not a pkg git dir"
    pkg <- Package <$> getDirectoryName
    withPackageDir (".", pkg)
    else do
    when (length pkgs > 1 && brnchs == BranchList []) $
      error' "At least one branch must be specified when there are multiple packages"
    mapM_ (withPackageDir . packagePath) pkgs
  where
    -- FIXME support arbitrary (module) branches
    withPackageDir :: (FilePath, Package) -> IO ()
    withPackageDir (dir, pkg) =
      withExistingDirectory dir $ do
      haveGit <- isPkgGitDir
      mcurrentbranch <- if haveGit then Just <$> gitCurrentBranch
                        else return Nothing
      branches <- listOfBranches haveGit brnchs
      unless (quiet || dir == "." || length branches == 1) $
        putPkgHdr pkg
      when haveGit $ do
        when clean checkWorkingDirClean
        unless quiet $ git_ "fetch" []
      when (brnchs == AllBranches) $
        putStrLn $ "Branches: " ++ unwords (map show branches) ++ "\n"
      mapM_ (action pkg) branches
      when (length branches /= 1) $
        whenJust mcurrentbranch gitSwitchBranch

-- -- do branch over packages
-- withBranchByPackages :: (Branch -> [String] -> IO ()) -> (Branches,[String]) -> IO ()
-- withBranchByPackages action (brs,pkgs) = do
--   when (null brs) $
--     error' "Please specify at least one branch"
--   when (null pkgs) $
--     error' "Please give at least one package"
--   forM_ brs $ \ br ->
--     -- forM_ (map packagePath pkgs) $ \ (dir,pkg) ->
--     -- withExistingDirectory dir $ do
--     -- void $ setupGit False pkg True
--     action br pkgs

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

builtRpms :: Branch -> FilePath -> IO [FilePath]
builtRpms br spec = do
  dist <- branchDist br
  rpmspec ["--builtrpms", "--define", "dist " ++ rpmDistTag dist] (Just "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") spec

-- from fedora-haskell-tools
buildRequires :: FilePath -> IO [String]
buildRequires spec =
  -- FIXME should resolve meta
  map (head . words) <$> rpmspec ["--buildrequires"] Nothing spec

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", "--whatprovides", pkg]

pkgInstalled :: String -> IO Bool
pkgInstalled pkg =
  cmdBool "rpm" ["--quiet", "-q", pkg]
