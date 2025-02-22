module RpmBuild (
  builtRpms,
  buildRPMs,
  buildRPMsNoBranch,
  installDeps,
  buildRequires,
  getSources,
  getSourcesMacros,
  getDynSourcesMacros,
  generateSrpm,
  generateSrpm',
  generateSrpmNoDist,
  BCond(..),
  ForceShort(..),
  isShortCircuit,
  checkSourcesMatch,
  notInstalled,
  rpmEval,
  distRpmOptions
  )
where

import Control.Exception (uninterruptibleMask_)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.RPM
import Distribution.Fedora.Branch (branchDistTag, branchRelease)
import Distribution.Fedora.Release (releaseVersion)
import Network.HTTP.Directory (Manager, httpExists, httpManager)
import Safe (lastMay)
import SimpleCmd.Rpm
import SimplePrompt (promptEnter, yesNo)
import System.Console.Pretty
import System.IO.Extra (withTempDir)
import System.Posix.Files
import System.Process.Typed (proc, readProcessInterleaved)

import Branches
import Cmd.Update (updateSourcesPkg)
import Common
import Common.System
import Git
import Package

distOpt :: Branch -> IO [String]
distOpt br = do
  disttag <- branchDistTag br
  return ["--define", "dist" +-+ "%{?distprefix}" ++ disttag]

distOptAny :: AnyBranch -> IO [String]
distOptAny b =
  releaseSystemBranch b >>= distOpt

-- FIXME hardcoding
distRpmOptions :: Branch -> IO [String]
distRpmOptions br =
  map ("--define=" ++) <$> do
  case br of
    Rawhide -> do
      ver <- releaseVersion <$> branchRelease br
      return ["fedora" +-+ ver, "fc" ++ ver +-+ "1"]
    Fedora n ->
      return ["fedora" +-+ show n, "fc" ++ show n +-+ "1"]
    EPEL n ->
      return ["rhel" +-+ show n, "el" ++ show n +-+ "1"]
    EPELNext n ->
      return ["rhel" +-+ show n, "el" ++ show n +-+ "1"]

-- branchToRelease :: AnyBranch -> IO Release
-- branchToRelease (RelBranch br) = branchRelease br
-- branchToRelease (OtherBranch b) = error' $ "no release for branch" +-+ b

builtRpms :: AnyBranch -> FilePath -> IO [FilePath]
builtRpms br spec = do
  distopt <- distOptAny br
  rpmdir <- fromMaybe "" <$> rpmEval "%{_rpmdir}"
  autoreleaseOpt <- getAutoReleaseOptions spec
  rpms <- rpmspec ("--builtrpms" : distopt ++ autoreleaseOpt) (Just (rpmdir </>  "%{arch}/%{name}-%{version}-%{release}.%{arch}.rpm")) spec
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
  (patches,srcs) <- partitionEithers . map sourcePatchFile
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
            if have_sources
              then grep_ src "sources"
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
    sourcePatchFile :: String -> Either FilePath FilePath
    sourcePatchFile field =
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
    doesSourceDirFileExist msrcdir file =
      doesFileExist (fromMaybe "" msrcdir </> file)

generateSrpm :: Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm = generateSrpm' False

generateSrpm' :: Bool -> Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpm' = generateSrpmNoDist False

generateSrpmNoDist :: Bool -> Bool -> Maybe AnyBranch -> FilePath -> IO FilePath
generateSrpmNoDist nodist force mbr spec = do
  srcs <- getSources spec
  distopt <-
    if nodist
    then return ["--define", "dist %{nil}"]
    else
      case mbr of
        Nothing -> return []
        Just br -> do
          rbr <- releaseSystemBranch br
          distOpt rbr
  msrcrpmdir <- rpmEval "%{_srcrpmdir}"
  autoreleaseOpt <- getAutoReleaseOptions spec
  srpmfile <- cmd "rpmspec" $ ["-q", "--srpm"] ++ distopt ++ autoreleaseOpt ++ ["--qf", fromMaybe "" msrcrpmdir </> "%{name}-%{version}-%{release}.src.rpm", spec]
  sourcediropt <- sourceDirCwdOpt
  let srcrpmdiropt = maybe [] (\dir -> ["--define", "_srcrpmdir" +-+ dir])
                     msrcrpmdir
      opts = distopt ++ sourcediropt ++ srcrpmdiropt
  if force then
    buildSrpm srpmfile opts
    else do
    exists <- doesFileExist srpmfile
    if not exists
      then buildSrpm srpmfile opts
      else do
      srpmTime <- getModificationTime srpmfile
      fileTimes <- mapM getModificationTime (spec:srcs)
      if any (srpmTime <) fileTimes
        then buildSrpm srpmfile opts
        else do
        -- pretty print with ~/
        putStrLn $ srpmfile +-+ "is up to date"
        return srpmfile
  where
    buildSrpm predicted opts = do
      autospec <- isRpmAutospec spec
      outs <-
        -- prevent Ctrl-c from truncating srpm to corrupted file
        fmap words . uninterruptibleMask_ $
        if autospec
        then cmd "fedpkg" $ "srpm" : opts
        else cmd "rpmbuild" (opts ++ ["-bs", spec])
      case filter ("src.rpm" `isExtensionOf`) outs of
        [srpm] -> do
          putStrLn $ "Created" +-+ takeFileName srpm
          unless (predicted == srpm) $
            warning $ "different to predicted NVR:" +-+ predicted
          return srpm
        srpms -> error' $ "could not determined generated srpm filename" +-+ unwords srpms

isRpmAutospec :: FilePath -> IO Bool
isRpmAutospec spec = do
  isAutoChangelog spec ||^ isAutoRelease spec

data BCond = BuildWith String | BuildWithout String

instance Show BCond where
  show (BuildWith s) = "--with=" ++ s
  show (BuildWithout s) = "--without=" ++ s

getAutoReleaseOptions :: FilePath -> IO [String]
getAutoReleaseOptions spec = do
      autorelease <- isAutoRelease spec
      if autorelease
        then do
        calculated <- calculateRelease spec
        return ["--define", "_rpmautospec_release_number" +-+ calculated]
        else return []

data ForceShort = ForceBuild | ShortCompile | ShortInstall
  deriving Eq

isShortCircuit :: Maybe ForceShort -> Bool
isShortCircuit ms =
  case ms of
    Just s -> s /= ForceBuild
    Nothing -> False

-- FIXME create build.log
-- Note does not check if bcond changed
-- FIXME check tarball timestamp
-- FIXME handle prep (-bp) too?
buildRPMs :: Bool -> Bool -> Bool -> Maybe Natural -> Maybe ForceShort
          -> [BCond] -> [FilePath] -> AnyBranch -> FilePath -> IO Bool
buildRPMs quiet debug noclean mjobs mforceshort bconds rpms br spec = do
  needBuild <-
    if isJust mforceshort
    then return True
    else do
      allexist <- and <$> mapM doesFileExist rpms
      if not allexist
        then return True
        else do
        specTime <- getModificationTime spec
        rpmTimes <- sort <$> mapM getModificationTime rpms
        case rpmTimes of
          [] -> return True -- corner case
          (rpmtime:_) -> return $ specTime > rpmtime
  if not needBuild then
    putStrLn "Existing rpms are newer than spec file (use --force to rebuild)"
    else do
    installDeps True spec
    void $ getSources spec
    distopt <- distOptAny br
    autoreleaseOpt <- getAutoReleaseOptions spec
    sourcediropt <- sourceDirCwdOpt
    let buildopt =
          case mforceshort of
            Just ShortCompile -> ["-bc", "--short-circuit"]
            Just ShortInstall -> ["-bi", "--short-circuit"]
            _ -> "-bb" : ["--noclean" | noclean]
        jobs =
          case mjobs of
            Nothing -> []
            Just n -> ["--define", "_smp_ncpus_max" +-+ show n]
        args = sourcediropt ++ distopt ++
               buildopt ++ jobs ++ map show bconds ++ autoreleaseOpt ++ [spec]
    date <- cmd "date" ["+%T"]
    rbr <- anyBranchToRelease br
    nvr <- do
      mnvr <- pkgNameVerRelDist False (Just rbr) spec
      case mnvr of
        Just nvr' -> return nvr'
        Nothing -> pkgNameVerRelNodist spec
    putStr $ date +-+ "Building" +-+ showNVR nvr +-+ "locally... "
    ok <- do
      let buildlog = ".build-" ++ (showVerRel . nvrVerRel) nvr <.> "log"
      whenM (doesFileExist buildlog) $ do
        let backup = buildlog <.> "prev"
        whenM (doesFileExist backup) $ do
          prevsize <- getFileSize backup
          currsize <- getFileSize buildlog
          when (prevsize > currsize) $
            copyFile backup (backup <.> "prev")
        copyFile buildlog (buildlog <.> "prev")
      -- FIXME also backup successful buildtree (if log ends in "+ exit 0")
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
      error' $ showNVR nvr +-+ "failed to build"
  return needBuild

quoteArg :: String -> String
quoteArg cs =
  if ' ' `elem` cs then '\'' : cs ++ "'" else cs

buildRPMsNoBranch :: Bool -> Bool -> Bool -> Maybe Natural -> Maybe ForceShort
                  -> [BCond] -> FilePath -> IO Bool
buildRPMsNoBranch quiet debug noclean mjobs mforceshort bconds spec = do
  installDeps True spec
  void $ getSources spec
  sourcediropt <- sourceDirCwdOpt
  let distopt = ["--define", "dist" +-+ "%{?distprefix}"]
      buildopt =
        case mforceshort of
          Just ShortCompile -> ["-bc", "--short-circuit"]
          Just ShortInstall -> ["-bi", "--short-circuit"]
          _ -> "-bb" : ["--noclean" | noclean]
      jobs =
        case mjobs of
          Nothing -> []
          Just n -> ["--define", "_smp_ncpus_max" +-+ show n]
      args = sourcediropt ++ distopt ++
             buildopt ++ jobs ++ map show bconds ++ [spec]
  date <- cmd "date" ["+%T"]
  nvr <- pkgNameVerRelNodist spec
  let buildlog = ".build-" ++ (showVerRel . nvrVerRel) nvr ++ "-HEAD" <.> "log"
  whenM (doesFileExist buildlog) $ do
    let backup = buildlog <.> "prev"
    whenM (doesFileExist backup) $ do
      prevsize <- getFileSize backup
      currsize <- getFileSize buildlog
      when (prevsize > currsize) $
        copyFile backup (backup <.> "prev")
    copyFile buildlog (buildlog <.> "prev")
  putStr $ date +-+ "Building" +-+ showNVR nvr +-+ "locally... "
  ok <- do
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
          else error' "failed to build"
        return res
  unless ok $
    error' $ showNVR nvr +-+ "failed to build"
  return True

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
        let rpmbuildArgs = ["-br", "--nodeps", spec] ++
                           ["--define", "_srcrpmdir" +-+ tmpdir]
        -- errors for missing deps
        (_ret, out) <- readProcessInterleaved $ proc "rpmbuild" rpmbuildArgs
        -- Wrote: /current/dir/SRPMS/name-version-release.buildreqs.nosrc.rpm
        case lastMay (B.words out) of
          Nothing -> error' $ spec +-+ "could not generate source rpm for dynamic buildrequires"
          Just srpmbs -> do
            let srpm = B.unpack srpmbs
            unless ("buildreqs.nosrc.rpm" `isSuffixOf` srpm) $ do
              B.putStrLn out
              error' $ "failed to generated buildreqs.nosrc.rpm for" +-+ spec
            exists <- doesFileExist srpm
            if exists
              then cmdLines "rpm" ["-qp", "--requires", srpm]
              else error' $ srpm +-+ "does not exist!"
    else
      -- FIXME should resolve meta
      rpmspec ["--buildrequires"] Nothing spec
  return $ brs ++ ["rpmautospec" | autorelease]
  where
    primary dep =
      case words dep of
        [] -> error' "empty dep" -- corner case
        (('(':h):_) -> Just h
        (d:_) -> if "rpmlib(" `isPrefixOf` d
             then Nothing
             else Just d

checkSourcesMatch :: Package -> AnyBranch -> FilePath -> IO ()
checkSourcesMatch pkg br spec = do
  -- "^[Ss]ource[0-9]*:"
  sourcefiles <- map (takeFileName . last . words) <$> cmdLines "spectool" [spec]
  sources <- do
    exists <- doesFileExist "sources"
    if exists
      then lines <$> readFile "sources"
      else return []
  gitfiles <- gitLines "ls-files" []
  let missing = filter (\src -> isNothing (find (src `isInfixOf`) sources) &&
                                src `notElem` gitfiles)
                sourcefiles
  unless (null missing) $ do
    -- FIXME maybe change to yesNo
    promptEnter $ color Red $ unwords missing +-+ "not in sources, press Enter to fix"
    -- FIXME check if already fixed before proceeding
    updateSourcesPkg False False True Nothing pkg br
    git_ "status" ["--short"]
    ok <- yesNo "Amend commit"
    when ok $ git_ "commit" ["--amend"]
    unlessM isGitDirClean $
      error' "local changes remain (dirty)"
    checkOnBranch
    checkSourcesMatch pkg br spec
  mgr <- httpManager
  mapM_ (checkLookasideCache mgr) sources
  where
    checkLookasideCache :: Manager -> String -> IO ()
    checkLookasideCache mgr source = do
      let (file,url) =
            case words source of
              ("SHA512":('(':fileparen):"=":[hash]) ->
                let file' = dropSuffix ")" fileparen
                in (file', "https://src.fedoraproject.org/lookaside/pkgs" +/+ unPackage pkg +/+ file +/+ "sha512" +/+ hash +/+ file)
              [hash,file'] ->
                (file', "https://src.fedoraproject.org/lookaside/pkgs" +/+ unPackage pkg +/+ file +/+ "md5" +/+ hash +/+ file)
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
