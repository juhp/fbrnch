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
  ForceShort(..),
  buildRPMs,
  installDeps,
  prepPackage,
  getSources,
  putPkgHdr,
  putPkgBrnchHdr,
  putPkgAnyBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
  splitBranchesPkgs,
--  withBranchByPackages,
  withPackageByBranches,
  withPackageByBranches',
  zeroOneBranches,
  oneBranch,
  cleanGit,
  cleanGitFetch,
  cleanGitFetchActive,
  dirtyGit,
  dirtyGitFetch,
  Package(..),
  packageSpec,
  pkgNameVerRel,
  pkgNameVerRel',
  buildRequires,
  notInstalled,
  pkgInstalled,
  systemBranch,
  equivNVR
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

localBranchSpecFile :: Package -> AnyBranch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitRepo
  when gitdir $ do
    putPkgAnyBrnchHdr pkg br
    gitSwitchBranch br
  if gitdir
    then do
    let spec = packageSpec pkg
    ifM (doesFileExist spec)
      (return spec) $
      do
        putStrLn $ "spec file is not " ++ spec ++ "\n"
        findSpecfile
    else findSpecfile

rpmEval :: String -> IO (Maybe String)
rpmEval s = do
  res <- cmd "rpm" ["--eval", s]
  return $ if null res || res == s then Nothing else Just res

generateSrpm :: Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm mbr spec = do
  getSources spec
  distopt <- case mbr of
               Nothing -> return []
               Just br -> do
                 dist <- getBranchDist br
                 return ["--define", "dist " ++ rpmDistTag dist]
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", "%{name}-%{version}-%{release}.src.rpm", spec]
  srcrpmdir <-
    ifM isPkgGitRepo
      (return ".") $
      rpmEval "%{_srcrpmdir}" >>= maybe (error' "%_srcrpmdir undefined!") return
  let srpm = srcrpmdir </> srpmfile
      srpmdiropt = if null srcrpmdir then []
                   else ["--define", "_srcrpmdir " ++ srcrpmdir]
  sourcedir <-
    ifM isPkgGitRepo
      (return ".") $
      rpmEval "%{_sourcedir}" >>= maybe (error' "%_sourcedir undefined!") return
  let sourcediropt = if null sourcedir then []
                     else ["--define", "_sourcedir " ++ sourcedir]
  ifM (notM $ doesFileExist srpm)
    (buildSrpm (distopt ++ srpmdiropt ++ sourcediropt)) $
    do
    specTime <- getModificationTime spec
    srpmTime <- getModificationTime srpm
    if srpmTime > specTime
      then do
      -- pretty print with ~/
      putStrLn $ srpm ++ " is up to date"
      return srpm
      else buildSrpm (distopt ++ srpmdiropt ++ sourcediropt)
  where
    buildSrpm opts = do
      srpm <- last . words <$> cmd "rpmbuild" (opts ++ ["-bs", spec])
      putStrLn $ "Created " ++ takeFileName srpm
      return srpm

data ForceShort = ForceBuild | ShortCircuit
  deriving Eq

-- FIXME create build.log
buildRPMs :: Bool -> Maybe ForceShort -> [FilePath] -> AnyBranch -> FilePath
          -> IO ()
buildRPMs quiet mforceshort rpms br spec = do
  needBuild <-
    if isJust mforceshort
    then return True
    else
    ifM (not . and <$> mapM doesFileExist rpms)
    (return True) $
    do specTime <- getModificationTime spec
       rpmTimes <- sort <$> mapM getModificationTime rpms
       return $ specTime > head rpmTimes
  if not needBuild then
    putStrLn "Existing rpms are newer than spec file (use --force to rebuild)"
    else do
    installDeps spec
    void $ getSources spec
    dist <- getBranchDist br
    cwd <- getCurrentDirectory
    gitDir <- isGitRepo
    let shortcircuit = mforceshort == Just ShortCircuit
    let buildopt = if shortcircuit then ["-bi", "--short-circuit"] else ["-bb"]
        rpmdirs =
          [ "--define="++ mcr +-+ cwd | gitDir,
            mcr <- ["_builddir", "_rpmdir", "_srcrpmdir", "_sourcedir"]]
        args = rpmdirs ++ ["--define", "dist " ++ rpmDistTag dist] ++ buildopt ++ [spec]
    if not quiet || shortcircuit then
      cmd_ "rpmbuild" args
      else do
      putStr "Building locally: "
      cmdSilent_ "rpmbuild" args
      putStrLn "done"

installDeps :: FilePath -> IO ()
installDeps spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStrLn $ "Need: " ++ unwords missingdeps
    putStr "Running dnf builddep... "
    cmdSilent "/usr/bin/sudo" $ "/usr/bin/dnf":["builddep", "--assumeyes", spec]
    putStrLn "done"

prepPackage :: Package -> AnyBranch -> IO ()
prepPackage pkg br =
  ifM (doesFileExist "dead.package")
    (putStrLn "dead.package") $
    do
    spec <- localBranchSpecFile pkg br
    unlessM (doesFileExist spec) $
      error' $ spec ++ " not found"
    cwd <- getCurrentDirectory
    getSources spec
    gitDir <- isGitRepo
    let rpmdirs =
          [ "--define="++ mcr +-+ cwd | gitDir,
            mcr <- ["_builddir", "_sourcedir"]]
        args = rpmdirs ++ ["-bp", spec]
    case br of
      RelBranch rbr -> do
        nvr <- pkgNameVerRel' rbr spec
        putStr $ "Prepping " ++ nvr ++ ": "
      _ -> return ()
    cmdSilent_ "rpmbuild" args
    putStrLn "done"

getSources :: FilePath -> IO ()
getSources spec = do
    gitDir <- isGitRepo
    srcdir <- getSourceDir gitDir
    srcs <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
    unless gitDir $
      unlessM (doesDirectoryExist srcdir) $
      createDirectoryIfMissing True srcdir
    forM_ srcs $ \ src ->
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
  where
    sourceFieldFile :: String -> FilePath
    sourceFieldFile field =
      if null field then
        -- should be impossible
        error "empty source field!"
      else (takeFileName . last . words) field

    getSourceDir :: Bool -> IO FilePath
    getSourceDir gitDir =
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

putPkgAnyBrnchHdr :: Package -> AnyBranch -> IO ()
putPkgAnyBrnchHdr pkg br =
  putStrLn $ "\n== " ++ unPackage pkg ++ " " ++ show br ++ " =="

packagePath :: String -> (FilePath, Package)
packagePath path =
  (path, Package (takeFileName path))

packageSpec :: Package -> FilePath
packageSpec pkg = unPackage pkg <.> "spec"

-- taken from monadlist-0.0.2 (BSD)
spanM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a, [a])
spanM _ [] = return (mzero, [])
spanM p (x:xs) = do
  bool <- p x
  if bool
    then do
      (ys, zs) <- spanM p xs
      return (x!ys, zs)
    else return (mzero, x:xs)
  where
    (!) :: (MonadPlus p) => a -> p a -> p a
    x' ! y = return x' `mplus` y

splitBranchesPkgs :: Maybe BranchOpts -> [String] -> IO ([AnyBranch], [String])
splitBranchesPkgs mbrnchopts args = do
  (abrs,pkgs) <- spanM (notM . doesDirectoryExist) args
  let brs = map anyBranch abrs
  return $ case mbrnchopts of
    Nothing -> (brs,pkgs)
    Just _ | null brs -> (brs,pkgs)
           | otherwise -> error' "cannot specify branches with branch options"

data GitOpts =
  GitOpts
  { gitOptClean :: Bool
  , gitOptFetch :: Bool
  , gitOptActive :: Bool
  }

cleanGit, cleanGitFetch, cleanGitFetchActive, dirtyGit, dirtyGitFetch :: Maybe GitOpts
--                                   clean fetch active
cleanGit =            Just $ GitOpts True  False False
cleanGitFetch =       Just $ GitOpts True  True  False
cleanGitFetchActive = Just $ GitOpts True  True  True
dirtyGit =            Just $ GitOpts False False False
dirtyGitFetch =       Just $ GitOpts False True  False

zeroOneBranches, oneBranch :: Maybe ([AnyBranch] -> Bool, String)
zeroOneBranches = Just ((< 2) . length, "cannot specify more than one branch")
oneBranch = Just ((== 1) . length, "must specify one branch")

-- do package over branches
withPackageByBranches :: Maybe Bool
                      -> Maybe GitOpts
                      -> Maybe BranchOpts
                      -> Maybe ([AnyBranch] -> Bool, String)
                      -> (Package -> AnyBranch -> IO ())
                      -> [String]
                      -> IO ()
withPackageByBranches mheader mgitopts mbrnchopts mreqbr action args =
  splitBranchesPkgs mbrnchopts args >>=
    withPackageByBranches' mheader mgitopts mbrnchopts mreqbr action

withPackageByBranches' :: Maybe Bool
                       -> Maybe GitOpts
                       -> Maybe BranchOpts
                       -> Maybe ([AnyBranch] -> Bool, String)
                       -> (Package -> AnyBranch -> IO ())
                       -> ([AnyBranch], [String])
                       -> IO ()
withPackageByBranches' mheader mgitopts mbrnchopts mreqbr action (brs,pkgs) = do
  case mbrnchopts of
    Just _ ->
      unless (null brs) $
      error' "cannot specify branches and branch option together"
    Nothing ->
      case mreqbr of
        Just (required,brerr) ->
          unless (required brs) $ error' brerr
        Nothing -> return ()
  if null pkgs
    then do
    pkg <- Package <$> getDirectoryName
    withPackageDir (".", pkg)
    else do
    when (length pkgs > 1 && null brs) $
      error' "At least one branch must be specified when there are multiple packages"
    mapM_ (withPackageDir . packagePath) pkgs
  where
    -- FIXME support arbitrary (module) branches
    withPackageDir :: (FilePath, Package) -> IO ()
    withPackageDir (dir, pkg) =
      withExistingDirectory dir $ do
      haveGit <- isPkgGitRepo
      when (isJust mgitopts && not haveGit) $
        error' $ "Not a pkg git dir: " ++ unPackage pkg
      mcurrentbranch <- if haveGit then Just <$> gitCurrentBranch
                        else return Nothing
      let fetch = have gitOptFetch
      when ((mheader == Just True || isJust mheader && length brs > 1 || fetch) && dir /= ".") $
        putPkgHdr pkg
      when haveGit $
        when (have gitOptClean) checkWorkingDirClean
      when fetch gitFetchSilent
      branches <- listOfBranches haveGit (have gitOptActive) mbrnchopts brs
      when (mbrnchopts == Just AllBranches) $
        putStrLn $ "Branches: " ++ unwords (map show branches) ++ "\n"
      mapM_ (action pkg) branches
      when (length branches /= 1) $
        whenJust mcurrentbranch gitSwitchBranch

    have :: (GitOpts -> Bool) -> Bool
    have opt = maybe False opt mgitopts

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

clonePkg :: Bool -> Maybe Branch -> String -> IO ()
clonePkg quiet mbr pkg =
  ifM (doesDirectoryExist pkg)
    {-then-} (putStrLn $ pkg ++ "/ already exists") $
    {-else-} do
      let mbranch = case mbr of
            Nothing -> []
            Just br -> ["--branch", show br]
      fasid <- fasIdFromKrb
      unless quiet $
        putStr $ pkg ++ " "
      git_ "clone" $ ["--quiet"] ++ mbranch ++ ["ssh://" ++ fasid ++ "@pkgs.fedoraproject.org/rpms/" ++ pkg]

pkgNameVerRel :: Branch -> FilePath -> IO (Maybe String)
pkgNameVerRel br spec = do
  dist <- branchDist br
  -- workaround dist with bootstrap
  hostdist <- cmd "rpm" ["--eval", "%{dist}"]
  fmap (replace hostdist (rpmDistTag dist)) . listToMaybe <$> rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec

pkgNameVerRel' :: Branch -> FilePath -> IO String
pkgNameVerRel' br spec = do
  mnvr <- pkgNameVerRel br spec
  case mnvr of
    Nothing -> error' $ "rpmspec failed to parse " ++ spec
    Just nvr -> return nvr

builtRpms :: AnyBranch -> FilePath -> IO [FilePath]
builtRpms br spec = do
  dist <- getBranchDist br
  rpms <- rpmspec ["--builtrpms", "--define", "dist " ++ rpmDistTag dist] (Just "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm") spec
  if null rpms
    then error' $ spec ++ " does not seem to create any rpms"
    else return rpms

getBranchDist :: AnyBranch -> IO Dist
getBranchDist (RelBranch br) = branchDist br
getBranchDist (OtherBranch _) = systemBranch >>= branchDist

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


-- FIXME could be more strict about dist tag (eg .fcNN only)
equivNVR :: String -> String -> Bool
equivNVR nvr1 nvr2
  | nvr1 == nvr2 = True
  | length nvr1 /= length nvr2 = False
  | otherwise =
      -- (name-ver-rel,.dist)
      let (nvr, r) = splitExtension nvr1
          (nvr', r') = splitExtension nvr2
      in nvr == nvr' &&
           -- (dist,.more)
           let (r1,r1') = splitExtension $ tail r
               (r2,r2') = splitExtension $ tail r'
           -- allow differing dist
           in length r1 == length r2 && r1' == r2'
