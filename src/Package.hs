{-# LANGUAGE CPP #-}

module Package (
  builtRpms,
  clonePkg,
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  cleanChangelog,
  getChangeLog,
  getPackageName,
  getSummaryURL,
  findSpecfile,
  maybeFindSpecfile,
  localBranchSpecFile,
  generateSrpm,
  generateSrpm',
  BCond(..),
  ForceShort(..),
  buildRPMs,
  installDeps,
  checkSourcesMatch,
  getSources,
  putPkgHdr,
  putPkgBrnchHdr,
  putPkgAnyBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
--  splitBranchesPkgs,
--  withBranchByPackages,
  withPackageByBranches,
--  withPackageByBranches',
  withPackagesMaybeBranch,
  LimitBranches(..),
  cleanGit,
  cleanGitFetch,
  cleanGitFetchActive,
  dirtyGit,
  dirtyGitFetch,
  dirtyGitHEAD,
  Package(..),
  packageSpec,
  pkgNameVerRel,
  pkgNameVerRel',
  buildRequires,
  notInstalled,
  pkgInstalled,
  rpmInstalled,
  repoquery,
  equivNVR,
  nameOfNVR
  ) where

import Common
import Common.System

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.RPM
import Distribution.Fedora
import SimpleCmd.Rpm
import System.Console.Pretty
import System.Posix.Files

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

getChangeLog :: Maybe String -> FilePath -> IO String
getChangeLog mcontext spec = do
  clog <- cleanChangelog spec
  putStrLn ""
  putStrLn "```"
  putStrLn clog
  putStrLn "```"
  ifM (not <$> isTty)
    (return clog) $
    do
      userlog <- prompt $ "Press Enter to use above or input " ++ fromMaybe "change" mcontext ++ " summary now"
      return $ if null userlog then clog else userlog

cleanChangelog :: FilePath -> IO String
cleanChangelog spec = do
  cs <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
  let ls = lines cs
      no = length $ filter ("- " `isPrefixOf`) ls
  return $ if no == 1 then removePrefix "- " cs else cs

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

-- FIXME check spec filename/%name more carefully
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
    Nothing -> do
      dir <- getDirectoryName
      error' $ "No spec file found in: " ++ dir

maybeFindSpecfile :: IO (Maybe FilePath)
maybeFindSpecfile = fileWithExtension ".spec"
  where
    -- looks in dir for a unique file with given extension
    fileWithExtension :: String -> IO (Maybe FilePath)
    fileWithExtension ext = do
      files <- filter (ext `isExtensionOf`) <$> listDirectory "."
      case files of
        [] -> return Nothing
        [spec] -> return $ Just spec
        _ -> error' $ "No unique " ++ ext ++ " file found"

localBranchSpecFile :: Package -> AnyBranch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitRepo
  if gitdir
    then do
    gitSwitchBranch br
    let spec = packageSpec pkg
    ifM (doesFileExist spec)
      (return spec) $
      do
        mspec <- maybeFindSpecfile
        case mspec of
          Just spc -> do
            putStrLn $ "Warning: directory name differs from " ++ spc ++ "\n"
            return spc
          Nothing -> error' $ "No spec file for: " ++ unPackage pkg
    else findSpecfile

rpmEval :: String -> IO (Maybe String)
rpmEval s = do
  res <- cmd "rpm" ["--eval", s]
  return $ if null res || res == s then Nothing else Just res

-- rpmEval' :: String -> IO String
-- rpmEval' s = do
--   mres <- rpmEval s
--   fromMaybe (error' (show s ++ " undefined!")) mres

generateSrpm :: Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm = generateSrpm' False

generateSrpm' :: Bool -> Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm' force mbr spec = do
  srcs <- getSources spec
  distopt <- case mbr of
               Nothing -> return []
               Just br -> do
                 dist <- getBranchDist br
                 return ["--define", "dist " ++ rpmDistTag dist]
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", "%{name}-%{version}-%{release}.src.rpm", spec]
  cwd <- getCurrentDirectory
  let srpmdiropt = ["--define", "_srcrpmdir " ++ cwd]
      sourcediropt = ["--define", "_sourcedir " ++ cwd]
  if force then
    buildSrpm (distopt ++ srpmdiropt ++ sourcediropt)
    else do
    exists <- doesFileExist srpmfile
    if not exists
      then buildSrpm (distopt ++ srpmdiropt ++ sourcediropt)
      else do
      srpmTime <- getModificationTime srpmfile
      fileTimes <- mapM getModificationTime (spec:srcs)
      if any (srpmTime <) fileTimes
        then buildSrpm (distopt ++ srpmdiropt ++ sourcediropt)
        else do
        -- pretty print with ~/
        putStrLn $ srpmfile ++ " is up to date"
        return srpmfile
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
-- FIXME check tarball timestamp
buildRPMs :: Bool -> Maybe ForceShort -> [BCond] -> [FilePath] -> AnyBranch
          -> FilePath -> IO Bool
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
    installDeps True spec
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
      if not quiet || shortcircuit
      then do
        rbr <- anyBranchToRelease br
        nvr <- pkgNameVerRel' rbr spec
        pipeBool ("rpmbuild", args) ("tee", [".build-" ++ showNVRVerRel (readNVR nvr) <.> "log"])
      else do
        date <- cmd "date" ["+%T"]
        putStr $ date ++ " Building " ++ takeBaseName spec ++ " locally... "
        res <- cmdSilentBool "rpmbuild" args
        when res $ putStrLn "done"
        return res
    unless ok $
      error' $ takeBaseName spec ++ " failed to build"
  return needBuild

-- FIXME print unavailable deps
installDeps :: Bool -> FilePath -> IO ()
installDeps strict spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStrLn $ "Running dnf builddep " ++ unwords missingdeps
    cmd_ "/usr/bin/sudo" $ "/usr/bin/dnf":"--quiet":"builddep": ["--skip-unavailable" | not strict] ++ ["--assumeyes", spec]

checkSourcesMatch :: FilePath -> IO ()
checkSourcesMatch spec = do
  -- "^[Ss]ource[0-9]*:"
  sourcefiles <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
  sources <- lines <$> readFile "sources"
  gitfiles <- gitLines "ls-files" []
  let missing = filter (\src -> (isNothing (find (src `isInfixOf`) sources) && src `notElem` gitfiles)) sourcefiles
  unless (null missing) $ do
    prompt_ $ color Red $ unwords missing ++ " not in sources, please fix"
    checkOnBranch
    checkSourcesMatch spec

getSources :: FilePath -> IO [FilePath]
getSources spec = do
  gitDir <- isGitRepo
  cwd <- getCurrentDirectory
  -- FIXME fallback to ~/rpmbuild/SOURCES?
  msrcdir <- do
    msourcedir <- rpmEval "%{_sourcedir}"
    case msourcedir of
      Nothing -> return Nothing
      Just srcdir ->
        if msourcedir == Just cwd
        then return Nothing
        else do
          dir <- doesDirectoryExist srcdir
          if dir
            then return msourcedir
            else return Nothing
  (patches,srcs) <- partitionEithers . map sourceFieldFile
                    <$> cmdLines "spectool" ["-a", spec]
  forM_ srcs $ \ src -> do
    exists <- doesFileExist src
    inSrcdir <- doesSourceDirFileExist msrcdir src
    unless exists $ do
      if inSrcdir
        then maybeSourceDir createLink msrcdir src
        else do
        uploaded <-
          if gitDir then do
            have_sources <- doesFileExist "sources"
            if have_sources then
              grep_ src "sources"
              else return False
          else return False
        mfedpkg <- findExecutable "fedpkg"
        if uploaded && isJust mfedpkg
          then cmd_ "fedpkg" ["sources"]
          else do
          cmd_ "spectool" ["-g", "-S", spec]
          unlessM (doesSourceDirFileExist msrcdir src) $
            error' $ "download failed: " ++ src
    unless inSrcdir $
      whenJust msrcdir $ \srcdir ->
      createLink src (srcdir </> src)
  forM_ patches $ \patch ->
    unlessM (doesFileExist patch) $ do
    inSrcdir <- doesSourceDirFileExist msrcdir patch
    if inSrcdir
      then maybeSourceDir copyFile msrcdir patch
      else do
      cmd_ "spectool" ["-g", "-P", spec]
      unlessM (doesSourceDirFileExist msrcdir patch) $
        error' $ "missing patch: " ++ patch
  return $ srcs ++ patches
  where
    sourceFieldFile :: String -> Either FilePath FilePath
    sourceFieldFile field =
      case word1 field of
        (f,v) ->
          -- rpmdevtools 9.3 (spectool always lists --all)
          -- "Source0:" or "Patch1:"
          (case lower (dropWhileEnd isDigit (init f)) of
             "source" -> Right
             "patch" -> Left
             _ -> error' $! "illegal field: " ++ f)
          $ takeFileName v

    maybeSourceDir :: (FilePath -> FilePath -> IO ())
                  -> Maybe FilePath -> FilePath -> IO ()
    maybeSourceDir act mdir file =
      whenJust mdir $ \dir ->
      act (dir </> file) file

    doesSourceDirFileExist :: Maybe FilePath -> FilePath -> IO Bool
    doesSourceDirFileExist Nothing _ = return False
    doesSourceDirFileExist (Just srcdir) file =
      doesFileExist (srcdir </> file)

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

-- splitBranchesPkgs :: Bool -> Maybe BranchOpts -> Bool -> [String]
--                   -> IO ([AnyBranch], [String])
-- splitBranchesPkgs release mbrnchopts exists args = do
--   pkggit <- isPkgGitRepo
--   brPkgs <- mapM (toBrPkg pkggit) args
--   let (brs,pkgs) = brPkgsToBranchesPkgs brPkgs
--   return $ case mbrnchopts of
--     Just _ | brs /= [] -> error' "cannot specify branches with branch options"
--     _ -> (brs,pkgs)
--   where
--     toBrPkg :: Bool -> String -> IO BrPkg
--     toBrPkg gitdir str =
--       case anyBranch str of
--         rbr@(RelBranch _) -> return (IsBr rbr)
--         abr@(OtherBranch p) -> if release then return (IsPkg p)
--                else
--                  ifM (isPath str)
--                  (return $ IsPkg str) $
--                  if gitdir
--                  then return (IsBr abr)
--                  else return $ if exists
--                                then IsBr abr
--                                else IsPkg str
--       where
--         isPath :: FilePath -> IO Bool
--         isPath fp =
--           if ".spec" `isExtensionOf` fp
--           then do
--             exists' <- doesFileExist fp
--             unless exists' $ error' $ fp ++ " file not found"
--             return True
--           else do
--             exists' <- doesDirectoryExist fp
--             let ispath = '/' `elem` fp
--             when (not exists' && ispath) $
--               error' $ fp ++ " directory not found"
--             return exists'

--     brPkgsToBranchesPkgs :: [BrPkg] -> ([AnyBranch], [String])
--     brPkgsToBranchesPkgs brpkgs =
--       let (pbrs,ppkgs) = span isBranch brpkgs
--       in (map toBranch pbrs, map toPackage ppkgs)
--       where
--         isBranch :: BrPkg -> Bool
--         isBranch (IsBr _) = True
--         isBranch (Unknown _) = True
--         isBranch (IsPkg _) = False

--         toBranch :: BrPkg -> AnyBranch
--         toBranch (IsBr br) = br
--         toBranch (Unknown br) = OtherBranch br
--         toBranch (IsPkg p) = error' $ "can't map package to branch: " ++ p

--         toPackage :: BrPkg -> String
--         toPackage (IsPkg p) = p
--         toPackage (Unknown p) = p
--         toPackage (IsBr b) = error' $ "can't map branch to package: " ++ show b

data GitOpts =
  GitOpts
  { gitOptClean :: Bool
  , gitOptFetch :: Bool
  , gitOptActive :: Bool
  , gitOptHEAD :: Bool -- allow detached head/rebase state
  }

cleanGit, cleanGitFetch, cleanGitFetchActive, dirtyGit, dirtyGitFetch, dirtyGitHEAD :: Maybe GitOpts
--                                   clean fetch active HEAD
cleanGit =            Just $ GitOpts True  False False  False
cleanGitFetch =       Just $ GitOpts True  True  False  False
cleanGitFetchActive = Just $ GitOpts True  True  True   False
dirtyGit =            Just $ GitOpts False False False  False
dirtyGitFetch =       Just $ GitOpts False True  False  False
dirtyGitHEAD =        Just $ GitOpts False False False  True

data LimitBranches = AnyNumber | Zero | ZeroOrOne | ExactlyOne
  deriving Eq

-- do package over branches
-- withPackageByBranches :: Maybe Bool
--                       -> Maybe GitOpts
--                       -> Maybe BranchOpts
--                       -> Bool
--                       -> LimitBranches
--                       -> (Package -> AnyBranch -> IO ())
--                       -> [String]
--                       -> IO ()
-- withPackageByBranches mheader mgitopts mbrnchopts exists limitBranches action args = do
--   (brs,pkgs) <- splitBranchesPkgs (have gitOptActive) mbrnchopts exists args
--   let mheader' =
--         case mheader of
--           Nothing -> Nothing
--           Just _ | length pkgs < 2 && length brs < 2 && isNothing mbrnchopts -> Nothing
--           _ -> mheader
--   withPackageByBranches' mheader' mgitopts mbrnchopts limitBranches action (brs,pkgs)
--   where
--     have :: (GitOpts -> Bool) -> Bool
--     have opt = maybe False opt mgitopts


-- FIXME rename to withPackages*
-- FIXME countdown packages
withPackageByBranches :: Maybe Bool
                      -> Maybe GitOpts
                      -> LimitBranches
                      -> (Package -> AnyBranch -> IO ())
                      -> (BranchesReq,[String])
                      -> IO ()
withPackageByBranches mheader mgitopts limitBranches action (breq,pkgs) =
  if null pkgs
    then
    withPackageDir "."
    else do
    when (length pkgs > 1 && breq == Branches []) $
      case limitBranches of
        Zero -> return ()
        ZeroOrOne -> warning "Better to specify an explicit branch for multiple packages"
        _ -> error' "At least one branch must be specified when there are multiple packages"
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
        mcurrentbranch <-
          if haveGit
          then
            if have gitOptHEAD
            then do
              cur <- gitCurrentBranch'
              if cur == OtherBranch "HEAD"
                then warning "detached HEAD" >> return Nothing
                else return $ Just cur
            else Just <$> gitCurrentBranch
          else return Nothing
        brs <- listOfAnyBranches (haveGit && not (have gitOptHEAD)) (have gitOptActive) breq
        case limitBranches of
          ZeroOrOne | length brs > 1 ->
            -- FIXME: could be handled better (testcase: run long list of packages in wrong directory)
            error' $ "more than one branch given: " ++ unwords (map show brs)
          ExactlyOne | null brs ->
            error' "please specify one branch"
          ExactlyOne | length brs > 1 ->
            error' "please only specify one branch"
          _ -> return ()
        let fetch = have gitOptFetch
        when ((isJust mheader || fetch) && dir /= ".") $
          case brs of
            [br] -> when (fetch || mheader == Just True) $ putPkgAnyBrnchHdr pkg br
            _ -> when (fetch || isJust mheader) $ putPkgHdr pkg
        when haveGit $
          when (have gitOptClean) checkWorkingDirClean
        when fetch $ gitFetchSilent
        -- FIXME!! no branch restriction
        when (breq == BranchOpt AllBranches) $
          putStrLn $ "Branches: " ++ unwords (map show brs) ++ "\n"
        -- FIXME add newline at end?
        let action' p b = do
              when (isJust mheader && length brs > 1) $ putPkgAnyBrnchHdr p b
              action p b
        mapM_ (action' pkg) brs
        when (length brs /= 1) $
          whenJust mcurrentbranch gitSwitchBranch

    have :: (GitOpts -> Bool) -> Bool
    have opt = maybe False opt mgitopts

withPackagesMaybeBranch :: Maybe Bool
                        -> Maybe GitOpts
                        -> LimitBranches
                        -> (Package -> AnyBranch -> IO ())
                        -> (Maybe Branch,[String])
                        -> IO ()
withPackagesMaybeBranch mheader mgitopts limitBranches action (mbr, pkgs) =
  withPackageByBranches mheader mgitopts limitBranches action (Branches (maybeToList mbr),pkgs)

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
      putStr $
        if quiet
        then "cloning..."
        else "Cloning: " ++ pkg
      git_ "clone" $ ["--quiet"] ++ mbranch ++ ["ssh://" ++ fasid ++ "@pkgs.fedoraproject.org/rpms/" ++ pkg]
      putStrLn ""

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

rpmInstalled :: NVRA -> IO Bool
rpmInstalled rpm =
  cmdBool "rpm" ["--quiet", "-q", showNVRA rpm]

pkgInstalled :: String -> IO Bool
pkgInstalled pkg =
  cmdBool "rpm" ["--quiet", "-q", pkg]

repoquery :: Branch -> [String] -> IO String
repoquery br args =
  cmd "dnf" (["repoquery", "--quiet", "--releasever=" ++ branchVersion br] ++ args)

-- FIXME should be more strict about dist tag (eg .fcNN only)
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

-- FIXME: obsolete by using NVR
-- n-v-r -> n
nameOfNVR :: String -> String
nameOfNVR = removeSeg . removeSeg
  where
    removeSeg = init . dropWhileEnd (/= '-')

-- FIXME: drop when rpm-nvr has nvrVerRel
showNVRVerRel :: NVR -> String
showNVRVerRel (NVR _n vr) = showVerRel vr
