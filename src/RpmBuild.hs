module RpmBuild (
  builtRpms,
  buildRPMs,
  installDeps,
  buildRequires,
  getSources,
  getSourcesMacros,
  getDynSourcesMacros,
  generateSrpm,
  generateSrpm',
  BCond(..),
  ForceShort(..),
  isShortCircuit,
  checkSourcesMatch,
  notInstalled,
  rpmEval
  )
where

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.RPM
import Distribution.Fedora hiding (Fedora,EPEL,EPELNext)
import Network.HTTP.Directory (Manager, httpExists, httpManager)
import SimpleCmd.Rpm
import SimplePrompt (promptEnter)
import System.Console.Pretty
import System.IO.Extra (withTempDir)
import System.Posix.Files

import Branches
import Common
import Common.System
import Git
import Package

builtRpms :: AnyBranch -> FilePath -> IO [FilePath]
builtRpms br spec = do
  dist <- getBranchDist br
  -- previously was "" for pkggit
  rpmdir <- fromMaybe "" <$> rpmEval "%{_rpmdir}"
  rpms <- rpmspec ["--builtrpms", "--define", "dist" +-+ rpmDistTag dist] (Just (rpmdir </>  "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm")) spec
  if null rpms
    then error' $ spec +-+ "does not seem to create any rpms"
    else return rpms

rpmEval :: String -> IO (Maybe String)
rpmEval s = do
  res <- cmd "rpm" ["--eval", s]
  return $ if null res || res == s then Nothing else Just res

-- rpmEval' :: String -> IO String
-- rpmEval' s = do
--   mres <- rpmEval s
--   fromMaybe (error' (show s +-+ "undefined!")) mres

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
            error' $ "download failed:" +-+ src
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
        error' $ "missing patch:" +-+ patch
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
             _ -> error' $! "illegal field:" +-+ f)
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
            putStrLn $ "Running 'dnf install'" +-+ prog
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

generateSrpm :: Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm = generateSrpm' False

generateSrpm' :: Bool -> Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm' force mbr spec = do
  srcs <- getSources spec
  distopt <- case mbr of
               Nothing -> return []
               Just br -> do
                 dist <- getBranchDist br
                 return ["--define", "dist" +-+ rpmDistTag dist]
  msrcrpmdir <- rpmEval "%{_srcrpmdir}"
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ ["--qf", fromMaybe "" msrcrpmdir </> "%{name}-%{version}-%{release}.src.rpm", spec]
  cwd <- getCurrentDirectory
  let sourcediropt = ["--define", "_sourcedir" +-+ cwd]
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
        putStrLn $ srpmfile +-+ "is up to date"
        return srpmfile
  where
    buildSrpm opts = do
      srpm <- last . words <$> cmd "rpmbuild" (opts ++ ["-bs", spec])
      putStrLn $ "Created" +-+ takeFileName srpm
      return srpm

data ForceShort = ForceBuild | ShortCompile | ShortInstall
  deriving Eq

isShortCircuit :: Maybe ForceShort -> Bool
isShortCircuit ms =
  case ms of
    Just s -> s /= ForceBuild
    Nothing -> False

data BCond = BuildWith String | BuildWithout String

instance Show BCond where
  show (BuildWith s) = "--with=" ++ s
  show (BuildWithout s) = "--without=" ++ s

-- FIXME create build.log
-- Note does not check if bcond changed
-- FIXME check tarball timestamp
buildRPMs :: Bool -> Bool -> Bool -> Maybe ForceShort -> [BCond] -> [FilePath]
          -> AnyBranch -> FilePath -> IO Bool
buildRPMs quiet debug noclean mforceshort bconds rpms br spec = do
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
    let buildopt =
          case mforceshort of
            Just ShortCompile -> ["-bc", "--short-circuit"]
            Just ShortInstall -> ["-bi", "--short-circuit"]
            _ -> "-bb" : ["--noclean" | noclean]
        sourcediropt = ["--define", "_sourcedir" +-+ cwd]
        args = sourcediropt ++ ["--define", "dist" +-+ rpmDistTag dist] ++
               buildopt ++ map show bconds ++ [spec]
    date <- cmd "date" ["+%T"]
    rbr <- anyBranchToRelease br
    nvr <- pkgNameVerRel' rbr spec
    putStr $ date +-+ "Building" +-+ nvr +-+ "locally... "
    ok <- do
      let buildlog = ".build-" ++ (showVerRel . nvrVerRel . readNVR) nvr <.> "log"
      whenM (doesFileExist buildlog) $
        copyFile buildlog (buildlog <.> "prev")
      timeIO $
        if not quiet || isShortCircuit mforceshort
        then do
          putNewLn
          -- FIXME would like to have pipeOutErr
          let buildcmd = unwords $ "rpmbuild" : map quoteArg args ++ "|&" : "tee" : [buildlog +-+ "&& exit ${PIPESTATUS[0]}"]
          when debug $ putStrLn buildcmd
          shellBool buildcmd
        else do
          let buildcmd = unwords $ "rpmbuild" : map quoteArg args ++ [">&", buildlog]
          when debug $ putStrLn buildcmd
          res <- shellBool buildcmd
          if res
            then putStrLn "done"
            else cmd_ "tail" ["-n 100", buildlog]
          return res
    unless ok $
      error' $ nvr +-+ "failed to build"
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
    putStrLn $ "Running 'dnf install'" +-+ unwords missingdeps
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

-- from fedora-haskell-tools
buildRequires :: FilePath -> IO [String]
buildRequires spec = do
  autorelease <- isAutoRelease spec
  dynbr <- grep_ "^%generate_buildrequires" spec
  brs <- mapMaybe primary <$>
    if dynbr
    then do
      installMissingMacros spec
      void $ getSources spec
      withTempDir $ \tmpdir -> do
        let srpmdiropt = ["--define", "_srcrpmdir" +-+ tmpdir]
        (ok, out, err) <- cmdFull "rpmbuild" (["-br", "--nodeps", spec] ++ srpmdiropt) ""
        if ok
          then do
          -- Wrote: /current/dir/SRPMS/name-version-release.buildreqs.nosrc.rpm
          case words out of
            [] -> error' $ spec +-+ "could not generate source rpm for dynamic buildrequires"
            ws -> do
              let srpm = last ws
              exists <- doesFileExist srpm
              if exists
                then cmdLines "rpm" ["-qp", "--requires", last ws]
                else error' err
          else error' err
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
    promptEnter $ color Red $ unwords missing +-+ "not in sources, please fix"
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
        putStrLn $ url +-+ "not found"
        putStrLn $ "uploading" +-+ file +-+ "to lookaside source repo"
        fedpkg_ "upload" [file]

notInstalled :: String -> IO Bool
notInstalled pkg =
  not <$> cmdBool "rpm" ["--quiet", "-q", "--whatprovides", pkg]

getSourcesMacros :: FilePath -> IO ()
getSourcesMacros spec = do
  void $ getSources spec
  installMissingMacros spec

getDynSourcesMacros :: FilePath -> IO ()
getDynSourcesMacros spec =
  whenM (grep_ "^%generate_buildrequires" spec) $
   getSourcesMacros spec
