{-# LANGUAGE CPP #-}

module Package (
  builtRpms,
  clonePkg,
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  cleanChangelog,
  getChangeLog,
  getDirectoryName,
  getPackageName,
  getSummaryURL,
  findSpecfile,
  localBranchSpecFile,
  generateSrpm,
  BCond(..),
  ForceShort(..),
  buildRPMs,
  installDeps,
  prepPackage,
  checkSourcesMatch,
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
  LimitBranches(..),
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
  equivNVR,
  takeNVRName,
  nameOfNVR
  ) where

import Common
import Common.System

import Distribution.Fedora
import SimpleCmd.Rpm
import System.Console.Pretty

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
  putStrLn ""
  putStrLn "```"
  putStrLn clog
  putStrLn "```"
  ifM (not <$> isTty)
    (return clog) $
    do
      userlog <- prompt "Press Enter to use above or input change summary now"
      return $ if null userlog then clog else userlog

cleanChangelog :: String -> String
cleanChangelog [] = error' "empty changelog" -- should not happen
cleanChangelog cs =
  let ls = lines cs
      no = length $ filter ("- " `isPrefixOf`) ls
  in if no == 1 then removePrefix "- " cs else cs

getSummaryURL :: FilePath -> IO String
getSummaryURL spec = do
  notes <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{summary}\n\n- %{url}", spec]
  putStrLn ""
  putStrLn "```"
  putStrLn notes
  putStrLn "```"
  ifM (not <$> isTty)
    (return notes) $
    do
      usernote <- prompt "Press Enter to use above or input notes"
      return $ if null usernote then notes else usernote

-- FIXME maybe check spec filename/%name more carefully
getPackageName :: FilePath -> IO Package
getPackageName pkgdir =
  if pkgdir == "."
  then Package <$> getDirectoryName
  else return $ Package $ takeFileName pkgdir

findSpecfile :: IO FilePath
findSpecfile = do
  mspec <- maybeFindSpecfile
  case mspec of
    Just spec -> return spec
    Nothing -> error' "No spec file found"

maybeFindSpecfile :: IO (Maybe FilePath)
maybeFindSpecfile = fileWithExtension ".spec"
  where
    -- looks in dir for a unique file with given extension
    fileWithExtension :: String -> IO (Maybe FilePath)
    fileWithExtension ext = do
      files <- filter ((== ext) . takeExtension) <$> listDirectory "."
      case files of
        [] -> return Nothing
        [spec] -> return $ Just spec
        _ -> error' $ "No unique " ++ ext ++ " file found"

localBranchSpecFile :: Package -> AnyBranch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitRepo
  if gitdir
    then do
    putPkgAnyBrnchHdr pkg br
    gitSwitchBranch br
    else putPkgHdr pkg
  if gitdir
    then do
    let spec = packageSpec pkg
    ifM (doesFileExist spec)
      (return spec) $
      do
        mspec <- maybeFindSpecfile
        case mspec of
          Just spc -> do
            putStrLn $ "Warning: directory name differs from " ++ spc ++ "\n"
            return spc
          Nothing -> error' "No spec file"
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

data BCond = BuildWith String | BuildWithout String

instance Show BCond where
  show (BuildWith s) = "--with=" ++ s
  show (BuildWithout s) = "--without=" ++ s

-- FIXME create build.log
-- Note does not check if bcond changed
buildRPMs :: Bool -> Maybe ForceShort -> [BCond] -> [FilePath] -> AnyBranch
          -> FilePath -> IO ()
buildRPMs quiet mforceshort bconds rpms br spec = do
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
        args = rpmdirs ++ ["--define", "dist " ++ rpmDistTag dist] ++ buildopt ++ map show bconds ++ [spec]
    ok <-
      if not quiet || shortcircuit then
      cmdBool "rpmbuild" args
      else do
      putStr "Building locally: "
      res <- cmdSilentBool "rpmbuild" args
      when res $ putStrLn "done"
      return res
    unless ok $
      error' $ "build failed for: " ++ takeBaseName spec

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
    cmdSilent' "rpmbuild" args
    putStrLn "done"

checkSourcesMatch :: FilePath -> IO ()
checkSourcesMatch spec = do
  -- "^[Ss]ource[0-9]*:"
  sourcefiles <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
  sources <- lines <$> readFile "sources"
  gitfiles <- gitLines "ls-files" []
  forM_ sourcefiles $ \ src ->
    unless (isJust (find (src `isInfixOf`) sources) || src `elem` gitfiles) $ do
    prompt_ $ color Red $ src ++ " not in sources, please fix"
    checkSourcesMatch spec

getSources :: FilePath -> IO ()
getSources spec = do
    gitDir <- isGitRepo
    srcdir <- getSourceDir gitDir
    srcs <- mapMaybe sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
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
          else do
          cmd_ "spectool" ["-g", "-S", "-C", srcdir, spec]
          unlessM (doesFileExist (srcdir </> src)) $
            error' $ "download failed: " ++ src
  where
    sourceFieldFile :: String -> Maybe FilePath
    sourceFieldFile field =
      case words field of
        [f,v] ->
          -- workaround rpmdevtools 9.3 (spectool -S lists patches)
          if "source" `isPrefixOf` lower f
          then Just $ takeFileName v
          else Nothing
        _ -> error $ "bad source field!: " ++ field

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
  deriving Eq

putPkgHdr :: Package -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n= " ++ unPackage pkg ++ " ="

putPkgBrnchHdr :: Package -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n== " ++ unPackage pkg ++ " " ++ show br ++ " =="

putPkgAnyBrnchHdr :: Package -> AnyBranch -> IO ()
putPkgAnyBrnchHdr pkg br =
  putStrLn $ "\n== " ++ unPackage pkg ++ " " ++ show br ++ " =="

packageSpec :: Package -> FilePath
packageSpec pkg = unPackage pkg <.> "spec"

data BrPkg = IsBr AnyBranch | Unknown String | IsPkg String
  deriving Show

splitBranchesPkgs :: Bool -> Maybe BranchOpts -> Bool -> [String]
                  -> IO ([AnyBranch], [String])
splitBranchesPkgs release mbrnchopts exists args = do
  pkggit <- isPkgGitRepo
  brPkgs <- mapM (toBrPkg pkggit) args
  let (brs,pkgs) = brPkgsToBranchesPkgs brPkgs
  return $ case mbrnchopts of
    Just _ | brs /= [] -> error' "cannot specify branches with branch options"
    _ -> (brs,pkgs)
  where
    toBrPkg :: Bool -> String -> IO BrPkg
    toBrPkg gitdir str =
      case anyBranch str of
        rbr@(RelBranch _) -> return (IsBr rbr)
        abr@(OtherBranch p) -> if release then return (IsPkg p)
               else
                 ifM (isPath str)
                 (return $ IsPkg str) $
                 if gitdir
                 then return (IsBr abr)
                 else return $ if exists
                               then IsBr abr
                               else IsPkg str
      where
        isPath :: FilePath -> IO Bool
        isPath fp =
          if ".spec" `isExtensionOf` fp
          then do
            exists' <- doesFileExist fp
            unless exists' $ error' $ fp ++ " file not found"
            return True
          else do
            exists' <- doesDirectoryExist fp
            let ispath = '/' `elem` fp
            when (not exists' && ispath) $
              error' $ fp ++ " directory not found"
            return exists'

    brPkgsToBranchesPkgs :: [BrPkg] -> ([AnyBranch], [String])
    brPkgsToBranchesPkgs brpkgs =
      let (pbrs,ppkgs) = span isBranch brpkgs
      in (map toBranch pbrs, map toPackage ppkgs)
      where
        isBranch :: BrPkg -> Bool
        isBranch (IsBr _) = True
        isBranch (Unknown _) = True
        isBranch (IsPkg _) = False

        toBranch :: BrPkg -> AnyBranch
        toBranch (IsBr br) = br
        toBranch (Unknown br) = OtherBranch br
        toBranch (IsPkg p) = error' $ "can't map package to branch: " ++ p

        toPackage :: BrPkg -> String
        toPackage (IsPkg p) = p
        toPackage (Unknown p) = p
        toPackage (IsBr b) = error' $ "can't map branch to package: " ++ show b

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

data LimitBranches = AnyNumber | ZeroOrOne | ExactlyOne
  deriving Eq

-- do package over branches
withPackageByBranches :: Maybe Bool
                      -> Maybe GitOpts
                      -> Maybe BranchOpts
                      -> Bool
                      -> LimitBranches
                      -> (Package -> AnyBranch -> IO ())
                      -> [String]
                      -> IO ()
withPackageByBranches mheader mgitopts mbrnchopts exists limitBranches action args = do
  (brs,pkgs) <- splitBranchesPkgs (have gitOptActive) mbrnchopts exists args
  let mheader' =
        case mheader of
          Nothing -> Nothing
          Just _ | length pkgs < 2 && length brs < 2 -> Nothing
          _ -> mheader
  withPackageByBranches' mheader' mgitopts mbrnchopts limitBranches action (brs,pkgs)
  where
    have :: (GitOpts -> Bool) -> Bool
    have opt = maybe False opt mgitopts

withPackageByBranches' :: Maybe Bool
                       -> Maybe GitOpts
                       -> Maybe BranchOpts
                       -> LimitBranches
                       -> (Package -> AnyBranch -> IO ())
                       -> ([AnyBranch], [String])
                       -> IO ()
withPackageByBranches' mheader mgitopts mbrnchopts limitBranches action (brs,pkgs) = do
  case mbrnchopts of
    Just _ ->
      unless (null brs) $
      error' "cannot specify branches and branch option together"
    Nothing ->
      case limitBranches of
        ZeroOrOne | length brs > 1 ->
          -- FIXME: could be handled better (testcase: run long list of packages in wrong directory)
          error' $ "more than one branch given or packages not found: " ++ unwords (tail (map show brs))
        ExactlyOne | null brs ->
          error' "please specify one branch"
        ExactlyOne | length brs > 1 ->
          error' "please only specify one branch"
        _ -> return ()
  if null pkgs
    then do
    withPackageDir "."
    else do
    when (length pkgs > 1 && null brs) $
      error' "At least one branch must be specified when there are multiple packages"
    mapM_ withPackageDir pkgs
  where
    -- FIXME support arbitrary (module) branches
    withPackageDir :: FilePath -> IO ()
    withPackageDir path = do
     let dir =
           if ".spec" `isExtensionOf` path
           then takeDirectory path
           else path
     withExistingDirectory dir $ do
      mspec <- if ".spec" `isExtensionOf` path
              then return $ Just $ takeFileName path
              else maybeFindSpecfile
      pkg <- Package <$>
             case mspec of
               -- FIXME fails if spec file can't be parsed and also is *slow*
               -- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{name}", spec]
               -- For now assume spec filename = package name
               Just spec -> return $ takeBaseName spec
               Nothing -> getDirectoryName

      unless (isNothing mspec || mspec == Just (unPackage pkg <.> "spec")) $
        putStrLn  "Warning: package name and spec filename differ!"
      haveGit <- isPkgGitRepo
      when (isJust mgitopts && not haveGit) $
        error' $ "Not a pkg git dir: " ++ unPackage pkg
      mcurrentbranch <- if haveGit then Just <$> gitCurrentBranch
                        else return Nothing
      let fetch = have gitOptFetch
      when ((mheader == Just True || isJust mheader && max (length brs) (length pkgs) > 1 || fetch) && dir /= ".") $
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

-- FIXME?
-- n-v-r.src.rpm -> n-v-r
takeNVRName :: FilePath -> String
takeNVRName = takeBaseName . takeBaseName

-- n-v-r -> n
nameOfNVR :: String -> String
nameOfNVR = removeSeg . removeSeg
  where
    removeSeg = init . dropWhileEnd (/= '-')

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
