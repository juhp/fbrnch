{-# LANGUAGE CPP #-}

module Package (
  builtRpms,
  clonePkg,
  CloneUser(..),
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  cleanChangelog,
  changelogVersions,
  changeLogPrompt,
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
  installMissingMacros,
  checkSourcesMatch,
  getSources,
  putPkgHdr,
  putPkgBrnchHdr,
  putPkgAnyBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
  withPackageByBranches,
  withPackagesMaybeBranch,
  withPackagesMaybeBranchNoHeadergit,
  LimitBranches(..),
  cleanGit,
  cleanGitActive,
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

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.RPM
import Distribution.Fedora hiding (Fedora,EPEL)
import Network.HTTP.Directory (Manager, httpExists, httpManager)
import SimpleCmd.Rpm
import System.Console.Pretty
import System.Posix.Files

import Branches
import Common
import Common.System
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

changeLogPrompt :: Maybe String -> FilePath -> IO String
changeLogPrompt mcontext spec = do
  clog <- cleanChangelog spec
  putStrLn ""
  putStrLn "```"
  putStrLn clog
  putStrLn "```"
  -- FIXME is this actually useful?
  tty <- isTty
  if not tty
    then return clog
    else do
    userlog <- prompt $ "Press Enter to use above or input " ++ fromMaybe "change" mcontext ++ " summary now"
    return $ if null userlog then clog else userlog

changelogVersions :: FilePath -> IO [String]
changelogVersions spec = do
  ns <- cmdLines "rpmspec" ["-q", "--srpm", "--qf", "%{changelogname}", spec]
  return $ map (removePrefix "- " . dropWhile (/= '-')) ns

cleanChangelog :: FilePath -> IO String
cleanChangelog spec = do
  autochangelog <- grep_ "^%autochangelog" spec
  ls <-
    if autochangelog
    then takeWhile (not . null) . drop 1 <$>
         cmdLines "rpmautospec" ["generate-changelog", spec]
    else cmdLines "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
  return $ case filter ("- " `isPrefixOf`) ls of
             [l] -> removePrefix "- " l
             _ -> unlines ls

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

-- adapted from fileWithExtension
maybeFindSpecfile :: IO (Maybe FilePath)
maybeFindSpecfile = do
  files <- filter (".spec" `isExtensionOf`) <$> listDirectory "."
  case files of
    [] -> return Nothing
    [spec] -> return $ Just spec
    _ -> error' $ "More than one .spec file: " ++ unwords files

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
  msrcrpmdir <- rpmEval "%{_srcrpmdir}"
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", fromMaybe "" msrcrpmdir </> "%{name}-%{version}-%{release}.src.rpm", spec]
  cwd <- getCurrentDirectory
  let sourcediropt = ["--define", "_sourcedir " ++ cwd]
      opts = distopt ++ sourcediropt
  if force then
    buildSrpm opts
    else do
    exists <- doesFileExist srpmfile
    if not exists
      then buildSrpm opts
      else do
      srpmTime <- getModificationTime srpmfile
      fileTimes <- mapM getModificationTime (spec:srcs)
      if any (srpmTime <) fileTimes
        then buildSrpm opts
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
    let shortcircuit = mforceshort == Just ShortCircuit
    let buildopt = if shortcircuit then ["-bi", "--short-circuit"] else ["-bb"]
        sourcediropt = ["--define", "_sourcedir " ++ cwd]
        args = sourcediropt ++ ["--define", "dist " ++ rpmDistTag dist] ++
               buildopt ++ map show bconds ++ [spec]
    ok <-
      if not quiet || shortcircuit
      then do
        rbr <- anyBranchToRelease br
        nvr <- pkgNameVerRel' rbr spec
        -- FIXME would like to have pipeOutErr
        timeIO $ shellBool $ intercalate " " $ "rpmbuild" : map quoteArg args ++ "|&" : "tee" : [".build-" ++ showNVRVerRel (readNVR nvr) <.> "log"]
      else do
        date <- cmd "date" ["+%T"]
        putStr $ date ++ " Building " ++ takeBaseName spec ++ " locally... "
        res <- timeIO $ cmdSilentBool "rpmbuild" args
        when res $ putStrLn "done"
        return res
    unless ok $
      error' $ takeBaseName spec ++ " failed to build"
  return needBuild
  where
    quoteArg :: String -> String
    quoteArg cs =
      if ' ' `elem` cs then '\'' : cs ++ "'" else cs

-- FIXME print unavailable deps
installDeps :: Bool -> FilePath -> IO ()
installDeps strict spec = do
  missingdeps <- nub <$> (buildRequires spec >>= filterM notInstalled)
  unless (null missingdeps) $ do
    putStrLn $ "Running 'dnf install' " ++ unwords missingdeps
    cmd_ "/usr/bin/sudo" $ "/usr/bin/dnf":"install": ["--skip-broken" | not strict] ++ ["--assumeyes"] ++ missingdeps

installMissingMacros :: FilePath -> IO ()
installMissingMacros spec = do
  macros <- mapMaybeM needSrpmMacro srpmMacros
  missing <- filterM notInstalled macros
  unless (null missing) $
    cmd_ "/usr/bin/sudo" $ ["/usr/bin/dnf", "install", "--assumeyes"] ++ missing
  where
    srpmMacros :: [(String,String)]
    srpmMacros =
      [("%gometa", "go-rpm-macros"),
       ("fontpkgname", "fonts-rpm-macros"),
       ("%cargo_prep", "rust-packaging")]

    needSrpmMacro :: (String,String) -> IO (Maybe String)
    needSrpmMacro (meta, macros) = do
      contents <- readFile spec
      return $ if meta `isInfixOf` contents then Just macros else Nothing

checkSourcesMatch :: FilePath -> IO ()
checkSourcesMatch spec = do
  -- "^[Ss]ource[0-9]*:"
  sourcefiles <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
  sources <- lines <$> readFile "sources"
  gitfiles <- gitLines "ls-files" []
  let missing = filter (\src -> isNothing (find (src `isInfixOf`) sources) &&
                                src `notElem` gitfiles)
                sourcefiles
  unless (null missing) $ do
    prompt_ $ color Red $ unwords missing ++ " not in sources, please fix"
    checkOnBranch
    checkSourcesMatch spec
  mgr <- httpManager
  let pkg = takeBaseName spec
  mapM_ (checkLookasideCache mgr pkg) sources
  where
    checkLookasideCache :: Manager -> String -> String -> IO ()
    checkLookasideCache mgr pkg source = do
      let (file,url) =
            case words source of
              ("SHA512":('(':fileparen):"=":[hash]) ->
                let file' = dropSuffix ")" fileparen
                in (file', "https://src.fedoraproject.org/lookaside/pkgs" +/+ pkg +/+ file +/+ "sha512" +/+ hash +/+ file)
              [hash,file'] ->
                (file', "https://src.fedoraproject.org/lookaside/pkgs" +/+ pkg +/+ file +/+ "md5" +/+ hash +/+ file)
              _ -> error' $ "invalid/unknown source:\n" ++ source
      unlessM (httpExists mgr url) $ do
        putStrLn $ url ++ " not found"
        putStrLn $ "uploading " ++ file ++ " to lookaside source repo"
        fedpkg_ "upload" [file]

getSources :: FilePath -> IO [FilePath]
getSources spec = do
  -- FIXME fallback to ~/rpmbuild/SOURCES?
  msrcdir <- do
    cwd <- getCurrentDirectory
    msourcedir <- rpmEval "%{_sourcedir}"
    case msourcedir of
      Nothing -> return Nothing
      Just srcdir -> do
        canon <- canonicalizePath srcdir
        if canon == cwd
          then return Nothing
          else do
          dir <- doesDirectoryExist srcdir
          if dir
            then return msourcedir
            else return Nothing
  isPkgGit <- isPkgGitRepo
  (patches,srcs) <- partitionEithers . map sourceFieldFile
                    <$> cmdLines "spectool" ["-a", spec]
  forM_ srcs $ \ src -> do
    exists <- doesFileExist src &&^ checkCompression src
    inSrcdir <- doesSourceDirFileExist msrcdir src
    unless exists $ do
      if inSrcdir
        then maybeSourceDir createLink msrcdir src
        else do
        uploaded <-
          if isPkgGit then do
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
          unlessM (doesFileExist src) $
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
      unlessM (doesFileExist patch) $
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

    checkCompression :: FilePath -> IO Bool
    checkCompression file =
      case
        case takeExtension file of
          ".gz" -> Just "gzip"
          ".tgz" -> Just "gzip"
          ".bz2" -> Just "bzip2"
          ".xz" -> Just "xz"
          ".lz" -> Just "lzip"
          ".zstd" -> Just "zstd"
          _ -> Nothing
      of
        Just prog -> do
          have <- findExecutable prog
          when (isNothing have) $ do
            putStrLn $ "Running 'dnf install' " ++ prog
            cmd_ "/usr/bin/sudo" $ "/usr/bin/dnf":"install": ["--assumeyes", prog]
          cmdBool prog ["-t", file]
        Nothing -> return True

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
withExistingDirectory dir act = do
  exists <- doesDirectoryExist dir
  if exists
  then withCurrentDirectory dir act
  else error' $ "No such directory: " ++ dir

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

cleanGit, cleanGitActive, cleanGitFetch, cleanGitFetchActive, dirtyGit, dirtyGitFetch, dirtyGitHEAD :: Maybe GitOpts
--                                   clean fetch active HEAD
cleanGit =            Just $ GitOpts True  False False  False
cleanGitActive =      Just $ GitOpts True  False True   False
cleanGitFetch =       Just $ GitOpts True  True  False  False
cleanGitFetchActive = Just $ GitOpts True  True  True   False
dirtyGit =            Just $ GitOpts False False False  False
dirtyGitFetch =       Just $ GitOpts False True  False  False
dirtyGitHEAD =        Just $ GitOpts False False False  True

data LimitBranches = AnyNumber | Zero | ZeroOrOne | ExactlyOne
  deriving Eq

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
        when fetch gitFetchSilent
        -- FIXME!! no branch restriction
        when (breq `elem` map BranchOpt [AllBranches,AllFedora,AllEPEL]) $
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

withPackagesMaybeBranchNoHeadergit :: LimitBranches
                                       -> (Package -> AnyBranch -> IO ())
                                       -> (Maybe Branch,[String])
                                       -> IO ()
withPackagesMaybeBranchNoHeadergit =
  withPackagesMaybeBranch Nothing Nothing

data CloneUser = AnonClone | UserClone

clonePkg :: Bool -> CloneUser -> Maybe Branch -> String -> IO ()
clonePkg quiet cloneuser mbr pkg = do
  exists <- doesDirectoryExist pkg
  if exists
    then putStrLn $ pkg ++ "/ already exists"
    else do
    let mbranch = case mbr of
          Nothing -> []
          Just br -> ["--branch", show br]
    case cloneuser of
      AnonClone -> do
        msgout
        git_ "clone" $ ["--quiet"] ++ mbranch ++ ["https://src.fedoraproject.org/rpms/" ++ pkg <.> "git"]
      UserClone -> do
        fasid <- fasIdFromKrb
        msgout
        git_ "clone" $ ["--quiet"] ++ mbranch ++ ["ssh://" ++ fasid ++ "@pkgs.fedoraproject.org/rpms/" ++ pkg <.> "git"]
  where
    msgout =
      putStrLn $ if quiet then "cloning..." else "Cloning: " ++ pkg

pkgNameVerRel :: Branch -> FilePath -> IO (Maybe String)
pkgNameVerRel br spec = do
  disttag <- rpmDistTag <$> branchDist br
  -- workaround dist with bootstrap
  hostdist <- cmd "rpm" ["--eval", "%{dist}"]
  -- FIXME more precise regexp with "Release:"
  autorelease <- grep_ " %autorelease" spec
  fmap (replace hostdist disttag) . listToMaybe <$>
    if autorelease
    then do
      mautospec <- findExecutable "rpmautospec"
      when (isNothing mautospec) $
        error' "requires rpmautospec.."
      autorel <- last . words <$> cmd "rpmautospec" ["calculate-release", spec]
      rpmspec ["--srpm"] (Just ("%{name}-%{version}-" ++ autorel ++ disttag)) spec
    else rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec

pkgNameVerRel' :: Branch -> FilePath -> IO String
pkgNameVerRel' br spec = do
  mnvr <- pkgNameVerRel br spec
  case mnvr of
    Nothing -> error' $ "rpmspec failed to parse " ++ spec
    Just nvr -> return nvr

builtRpms :: AnyBranch -> FilePath -> IO [FilePath]
builtRpms br spec = do
  dist <- getBranchDist br
  rpmdir <- do
    pkggit <- isPkgGitRepo
    if pkggit
    then return ""
    else fromMaybe "" <$> rpmEval "%{_rpmdir}"
  rpms <- rpmspec ["--builtrpms", "--define", "dist " ++ rpmDistTag dist] (Just (rpmdir </>  "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm")) spec
  if null rpms
    then error' $ spec ++ " does not seem to create any rpms"
    else return rpms

getBranchDist :: AnyBranch -> IO Dist
getBranchDist (RelBranch br) = branchDist br
getBranchDist (OtherBranch _) = systemBranch >>= branchDist

-- from fedora-haskell-tools
buildRequires :: FilePath -> IO [String]
buildRequires spec = do
  autorelease <- grep_ " %autorelease" spec
  dynbr <- grep_ "^%generate_buildrequires" spec
  brs <- mapMaybe primary <$>
    if dynbr
    then do
      installMissingMacros spec
      out <- cmdIgnoreErr "rpmbuild" ["-br", "--nodeps", spec] ""
      -- Wrote: /current/dir/SRPMS/name-version-release.buildreqs.nosrc.rpm
      cmdLines "rpm" ["-qp", "--requires", last (words out)]
    else
      -- FIXME should resolve meta
      rpmspec ["--buildrequires"] Nothing spec
  return $ brs ++ ["rpmautospec" | autorelease]
  where
    primary dep =
      case (head . words) dep of
        '(':rest -> Just rest
        d -> if "rpmlib(" `isPrefixOf` d
             then Nothing
             else Just d

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", "--whatprovides", pkg]

rpmInstalled :: NVRA -> IO Bool
rpmInstalled rpm =
  cmdBool "rpm" ["--quiet", "-q", showNVRA rpm]

pkgInstalled :: String -> IO Bool
pkgInstalled pkg =
  cmdBool "rpm" ["--quiet", "-q", pkg]

repoquery :: Branch -> Branch -> [String] -> IO String
repoquery sysbr br args = do
  let brOpts =
        if sysbr == br
        then []
        else
          case br of
            Rawhide -> ["--disablerepo=*", "--enablerepo=rawhide"]
            Fedora _ -> ["--disablerepo=*", "--enablerepo=fedora",
                         "--enablerepo=updates",
                         "--releasever=" ++ branchVersion br]
            EPEL _ -> ["--disablerepo=*", "--enablerepo=epel",
                         "--releasever=" ++ branchVersion br]
  cmd "dnf" (["repoquery", "--quiet"] ++ brOpts ++ args)

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
