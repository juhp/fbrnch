{-# LANGUAGE CPP #-}

module Package (
  clonePkg,
  CloneUser(..),
  fedpkg,
  fedpkg_,
  checkForSpecFile,
  getChangelog,
  cleanChangelog,
  changeLogPrompt,
  releaseSystemBranch,
  getPackageName,
  getSummaryURL,
  findSpecfile,
  maybeFindSpecfile,
  localBranchSpecFile,
  putPkgHdr,
  putPkgBrnchHdr,
  putPkgAnyBrnchHdr,
  withExistingDirectory,
  initialPkgRepo,
  withPackagesByBranches,
  withPackagesBranch,
  withPackagesMaybeBranch,
  withPackagesMaybeBranchNoHeadergit,
  HeaderShow(..),
  boolHeader,
  LimitBranches(..),
  cleanGit,
  cleanGitActive,
  cleanGitFetch,
  cleanGitFetchActive,
  dirtyGit,
  dirtyGitActive,
  dirtyGitFetch,
  dirtyGitHEAD,
  stashGitFetch,
  Package(..),
  packageSpec,
  pkgNameVerRel,
  pkgNameVerRel',
  pkgNameVerRelNodist,
  equivNVR,
  editSpecField,
  isAutoRelease
  sourceDirCwdOpt
  ) where

import Data.RPM (NV(..), VerRel(..))
import Data.RPM.NVR (maybeNVR, NVR(..))
import Distribution.Fedora.Branch
import SimpleCmd.Rpm
import SimplePrompt (prompt, promptInitial)

import Branches
import Common
import Common.System
import Git
import Krb
import Types (ChangeType(..))

fedpkg :: String -> [String] -> IO String
fedpkg c args =
  cmd "fedpkg" (c:args)

fedpkg_ :: String -> [String] -> IO ()
fedpkg_ c args =
  cmd_ "fedpkg" (c:args)

checkForSpecFile :: String -> IO ()
checkForSpecFile spec = do
  have <- doesFileExist spec
  unless have $ error' $ spec +-+ "not found"

-- FIXME allow editor to be used
changeLogPrompt :: ChangeType -> FilePath -> IO String
changeLogPrompt change spec = do
  clog <- cleanChangelog (change /= ChangeReview) spec
  putNewLn
  putStrLn "```"
  putStrLn clog
  putStrLn "```"
  -- FIXME is this actually useful?
  tty <- isTty
  if not tty
    then return clog
    else do
    -- FIXME ask what to do: edit/skip/append?
    userlog <- promptInitial
               ("Describe" +-+
                 case change of
                   ChangeBodhi -> "update"
                   ChangeCommit -> "commit"
                   ChangeReview -> "change"
                 +-+ "summary now"
                 ++ (if change == ChangeBodhi then "; or 'no' to skip update" else "")
                 ++ ":\n")
               clog
    return $ if null userlog then clog else userlog

getChangelog :: FilePath -> IO [String]
getChangelog spec = do
  autochangelog <- grep_ "^%autochangelog" spec
  if autochangelog
    then takeWhile (not . null) . drop 1 <$>
         cmdLines "rpmautospec" ["generate-changelog", spec]
    else cmdLines "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]

cleanChangelog :: Bool -> FilePath -> IO String
cleanChangelog nobullet spec = do
  ls <- getChangelog spec
  return $
    case filter ("- " `isPrefixOf`) ls of
      [l] | nobullet -> removePrefix "- " l
      _ -> intercalate "\n" ls

getSummaryURL :: FilePath -> IO String
getSummaryURL spec = do
  notes <- cmd "rpmspec" ["-q", "--srpm", "--qf", "%{summary}\n\n- %{url}", spec]
  putNewLn
  putStrLn "```"
  putStrLn notes
  putStrLn "```"
  tty <- isTty
  if tty
    then do
    usernote <- prompt "Press Enter to use above or input notes"
    return $ if null usernote then notes else usernote
    else return notes

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
      error' $ "No spec file found in:" +-+ dir

-- adapted from fileWithExtension
maybeFindSpecfile :: IO (Maybe FilePath)
maybeFindSpecfile = do
  files <- filter (".spec" `isExtensionOf`) <$> listDirectory "."
  case files of
    [] -> return Nothing
    [spec] -> return $ Just spec
    _ -> error' $ "More than one .spec file:" +-+ unwords files

localBranchSpecFile :: Package -> AnyBranch -> IO FilePath
localBranchSpecFile pkg br = do
  gitdir <- isPkgGitRepo
  if gitdir
    then do
    gitSwitchBranch br
    let spec = packageSpec pkg
    exists <- doesFileExist spec
    if exists
      then return spec
      else do
      mspec <- maybeFindSpecfile
      case mspec of
        Just spc -> do
          putStrLn $ "Warning: directory name differs from" +-+ spc ++ "\n"
          return spc
        Nothing -> error' $ "No spec file for:" +-+ unPackage pkg
    else findSpecfile

withExistingDirectory :: FilePath -> IO a -> IO a
withExistingDirectory dir act = do
  exists <- doesDirectoryExist dir
  if exists
  then withCurrentDirectory dir act
  else error' $ "No such directory:" +-+ dir

-- newly created Fedora repos/branches have just one README commit
initialPkgRepo :: IO Bool
initialPkgRepo = do
  commits <- length <$> gitShortLogN (Just 2) Nothing
  return $ commits <= 1

newtype Package = Package {unPackage :: String}
  deriving Eq

putPkgHdr :: Package -> IO ()
putPkgHdr pkg =
  putStrLn $ "\n=" +-+ unPackage pkg +-+ "="

putPkgBrnchHdr :: Package -> Branch -> IO ()
putPkgBrnchHdr pkg br =
  putStrLn $ "\n==" +-+ unPackage pkg +-+ show br +-+ "=="

putPkgAnyBrnchHdr :: Package -> AnyBranch -> IO ()
putPkgAnyBrnchHdr pkg br =
  putStrLn $ "\n==" +-+ unPackage pkg +-+ show br +-+ "=="

packageSpec :: Package -> FilePath
packageSpec pkg = unPackage pkg <.> "spec"

data GitOpts =
  GitOpts
  { gitOptClean :: Bool
  , gitOptFetch :: Bool
  , gitOptActive :: Bool
  , gitOptHEAD :: Bool -- allow detached head/rebase state
  , gitOptStash :: Bool
  }

cleanGit, cleanGitActive, cleanGitFetch, cleanGitFetchActive, dirtyGit, dirtyGitActive, dirtyGitFetch, dirtyGitHEAD, stashGitFetch :: Maybe GitOpts
--                                   clean fetch active HEAD  stash
cleanGit =            Just $ GitOpts True  False False  False False
cleanGitActive =      Just $ GitOpts True  False True   False False
cleanGitFetch =       Just $ GitOpts True  True  False  False False
cleanGitFetchActive = Just $ GitOpts True  True  True   False False
dirtyGit =            Just $ GitOpts False False False  False False
dirtyGitActive =      Just $ GitOpts False False True   False False
dirtyGitFetch =       Just $ GitOpts False True  False  False False
dirtyGitHEAD =        Just $ GitOpts False False False  True  False
stashGitFetch =       Just $ GitOpts True  True  False  False True

data LimitBranches = AnyNumber | Zero | ZeroOrOne | ExactlyOne
  deriving Eq

data HeaderShow = HeaderNone | HeaderMay | HeaderMust
  deriving Eq

boolHeader :: Bool -> HeaderShow
boolHeader b = if b then HeaderMust else HeaderMay

-- FIXME option to filter out dead.packages
withPackagesByBranches :: HeaderShow
                       -> Bool
                       -> Maybe GitOpts
                       -> LimitBranches
                       -> (Package -> AnyBranch -> IO ())
                       -> (BranchesReq,[String])
                       -> IO ()
withPackagesByBranches header count mgitopts limitBranches action (breq,pkgs) =
  if null pkgs
    then withPackageDir (0, ".")
    else do
    let numpkgs = length pkgs
    when (numpkgs > 1 && breq == Branches []) $
      case limitBranches of
        Zero -> return ()
        ZeroOrOne -> warning "Better to specify an explicit branch for multiple packages"
        _ -> error' "At least one branch must be specified when there are multiple packages"
    mapM_ withPackageDir $ zip [numpkgs,(numpkgs-1)..1] pkgs
  where
    -- FIXME support arbitrary (module) branches
    withPackageDir :: (Int,FilePath) -> IO ()
    withPackageDir (n, path) = do
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
        when (count && length pkgs >= 2) $
          putStrLn $ plural n "package" +-+ "left"
        unless (isNothing mspec || mspec == Just (unPackage pkg <.> "spec")) $
          putStrLn $ "Warning: package name (" ++ unPackage pkg ++ ") differs from spec filename!"
        haveGit <- isPkgGitRepo
        when (isJust mgitopts && not haveGit) $
          error' $ "Not a pkg git dir:" +-+ unPackage pkg
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
        let fetch = have gitOptFetch
        -- quiet to avoid output before header
        when fetch $ gitFetchSilent True
        brs <- listOfAnyBranches (haveGit && not (have gitOptHEAD)) (have gitOptActive) breq
        case limitBranches of
          ZeroOrOne | length brs > 1 ->
            -- FIXME: could be handled better (testcase: run long list of packages in wrong directory)
            error' $ "more than one branch given:" +-+ unwords (map show brs)
          ExactlyOne | null brs ->
            error' "please specify one branch"
          ExactlyOne | length brs > 1 ->
            error' "please only specify one branch"
          _ -> return ()
        when ((header /= HeaderNone || fetch) && dir /= ".") $
          case brs of
            [br] -> when (fetch || header == HeaderMust) $ putPkgAnyBrnchHdr pkg br
            _ -> when (fetch || header /= HeaderNone) $ putPkgHdr pkg
        when haveGit $
          when (have gitOptClean) $ checkWorkingDirClean (have gitOptStash)
        -- FIXME!! no branch restriction
        when (breq `elem` map BranchOpt [AllBranches,AllFedora,AllEPEL]) $
          putStrLn $ "Branches:" +-+ unwords (map show brs) ++ "\n"
        -- FIXME add newline at end?
        let action' p b = do
              when (header /= HeaderNone && length brs > 1) $ putPkgAnyBrnchHdr p b
              action p b
        mapM_ (action' pkg) brs
        when (length brs /= 1) $
          whenJust mcurrentbranch gitSwitchBranch

    have :: (GitOpts -> Bool) -> Bool
    have opt = maybe False opt mgitopts

withPackagesBranch :: HeaderShow
                   -> Bool
                   -> Maybe GitOpts
                   -> (Package -> AnyBranch -> IO ())
                   -> (Branch,[String])
                   -> IO ()
withPackagesBranch header count mgitopts action (br, pkgs) =
  withPackagesByBranches header count mgitopts ExactlyOne action (Branches [br],pkgs)

withPackagesMaybeBranch :: HeaderShow
                        -> Bool
                        -> Maybe GitOpts
                        -> (Package -> AnyBranch -> IO ())
                        -> (Maybe Branch,[String])
                        -> IO ()
withPackagesMaybeBranch header count mgitopts action (mbr, pkgs) =
  withPackagesByBranches header count mgitopts ZeroOrOne action (Branches (maybeToList mbr),pkgs)

withPackagesMaybeBranchNoHeadergit :: (Package -> AnyBranch -> IO ())
                                   -> (Maybe Branch,[String])
                                   -> IO ()
withPackagesMaybeBranchNoHeadergit =
  withPackagesMaybeBranch HeaderNone False Nothing

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
      putStrLn $ if quiet then "cloning..." else "Cloning:" +-+ pkg

isAutoRelease :: FilePath -> IO Bool
isAutoRelease spec = do
  matches <- filter ("Release:" `isPrefixOf`) <$> grep "%autorelease" spec
  return $ not (null matches)

pkgNameVerRel :: Branch -> FilePath -> IO (Maybe NVR)
pkgNameVerRel = pkgNameVerRelDist . Just

pkgNameVerRelDist :: Maybe Branch -> FilePath -> IO (Maybe NVR)
pkgNameVerRelDist Nothing spec = do
  nvrs <- rpmspec ["--srpm", "--undefine=dist"] (Just "%{name}-%{version}-%{release}") spec
  return $
    case nvrs of
      [] -> Nothing
      [nvr] -> maybeNVR nvr
      _ -> error' "could not determine unique nvr"
pkgNameVerRelDist (Just br) spec = do
  disttag <- branchDistTag br
  -- workaround dist with bootstrap
  hostdist <- cmd "rpm" ["--eval", "%{dist}"]
  -- FIXME more precise regexp with "Release:"
  autorelease <- isAutoRelease spec
  nvrs <- do
    pkggit <- isPkgGitRepo
    if autorelease && pkggit
    then do
      --putStrLn "%autorelease detected"
      mfedpkg <- findExecutable "fedpkg"
      when (isNothing mfedpkg) $
        error' "requires fedpkg.."
      cmdLines "fedpkg" ["verrel"]
    else rpmspec ["--srpm"] (Just "%{name}-%{version}-%{release}") spec
  seq disttag $
    return $
    case nvrs of
      [] -> Nothing
      [nvr] -> maybeNVR (replace hostdist disttag nvr)
      _ -> error' "could not determine unique nvr"

pkgNameVerRel' :: Branch -> FilePath -> IO NVR
pkgNameVerRel' br spec = do
  mnvr <- pkgNameVerRel br spec
  case mnvr of
    Nothing -> error' $ "rpmspec failed to parse" +-+ spec
    Just nvr -> return nvr

pkgNameVerRelNodist :: FilePath -> IO NVR
pkgNameVerRelNodist spec = do
  mnvr <- pkgNameVerRelDist Nothing spec
  case mnvr of
    Nothing -> error' $ "rpmspec failed to parse" +-+ spec
    Just nvr -> return nvr

releaseSystemBranch :: AnyBranch -> IO Branch
releaseSystemBranch (RelBranch br) = return br
releaseSystemBranch (OtherBranch _) = systemBranch

-- FIXME should be more strict about dist tag (eg .fcNN only)
equivNVR :: NVR -> Maybe NVR -> Bool
equivNVR _ Nothing = False
equivNVR nvr1 (Just nvr2) =
  nvr1 == nvr2
  ||
  let (nv1,r1) = splitRelease nvr1
      (nv2,r2) = splitRelease nvr2
  in
     nv1 == nv2
     &&
     compareReleases r1 r2
  where
    compareReleases :: String -> String -> Bool
    compareReleases r1 r2 =
      r1 == r2
      ||
      case ('.' `elem` r1, '.' `elem` r2) of
        (True,True) ->
          let (r1',drs1) = span (/= '.') r1
              (r2',drs2) = span (/= '.') r2
          in
            case (take 2 r1', take 2 r2') of
              ("fc","fc") -> True
              _ -> r1' == r2' && length drs1 == length drs2
            &&
            compareReleases (tail drs1) (tail drs2)
        _ -> False

-- data BrPkg = IsBr AnyBranch | Unknown String | IsPkg String
--   deriving Show

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
--             unless exists' $ error' $ fp +-+ "file not found"
--             return True
--           else do
--             exists' <- doesDirectoryExist fp
--             let ispath = '/' `elem` fp
--             when (not exists' && ispath) $
--               error' $ fp +-+ "directory not found"
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
--         toBranch (IsPkg p) = error' $ "can't map package to branch:" +-+ p

--         toPackage :: BrPkg -> String
--         toPackage (IsPkg p) = p
--         toPackage (Unknown p) = p
--         toPackage (IsBr b) = error' $ "can't map branch to package:" +-+ show b

editSpecField :: String -> String -> FilePath -> IO ()
editSpecField field new spec =
  cmd_ "sed" ["-i", "-e s/^\\(" ++ field ++ ":\\s\\+\\).*/\\1" ++ new ++ "/", spec]

#if !MIN_VERSION_rpm_nvr(0,1,3)
splitRelease :: NVR -> (NV,String)
splitRelease (NVR n (VerRel v r)) = (NV n v, r)
#endif

sourceDirCwdOpt :: IO [String]
sourceDirCwdOpt = do
  cwd <- getCurrentDirectory
  return ["--define", "_sourcedir" +-+ cwd]
