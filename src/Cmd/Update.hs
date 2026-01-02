module Cmd.Update
  ( updateSourcesCmd,
    updateSourcesPkg
  )
where

import Data.RPM.VerCmp
import Data.Version (parseVersion)
import Fedora.Krb (krbTicket)
import Safe (headMay, tailSafe)
import SimplePrompt (promptEnter, yesNoDefault)
import System.IO.Extra (withTempDir)
import Text.ParserCombinators.ReadP (readP_to_S)

import Branches
import Cmd.CompareTarballs
import Common
import Common.System
import Git
import InterleaveOutput (cmdSilent')
import Package

-- FIXME --no-prep to avoid overwriting ongoing build
-- FIXME don't bump release if already bumped
-- FIXME check EVR increased
-- FIXME if multiple sources might need to bump release
-- FIXME Haskell subpackages require release bump even with version bump
updateSourcesCmd :: Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
updateSourcesCmd force allowHEAD (mbr,args) = do
  (mver,pkgs) <-
        case args of
          [] -> return (Nothing,[])
          (h:t) -> do
            exists <- doesDirectoryExist h
            if exists || not (isVersion h)
              then return (Nothing, args)
              else do
              havespec <- isJust <$> maybeFindSpecfile
              if null t && not havespec
                then error' "not a pkg dir"
                else return (Just h, t)
  pkgGit <- isPkgGitSshRepo
  let mgitops =
        let dirty = if allowHEAD then dirtyGitHEAD else dirtyGitFetch
        in if pkgGit
           then dirty
           else if null pkgs then Nothing else dirty
  withPackagesMaybeBranch HeaderMay False mgitops (updateSourcesPkg force allowHEAD pkgGit mver) (mbr, pkgs)
  where
    isVersion = not . null . readP_to_S parseVersion

updateSourcesPkg :: Bool -> Bool -> Bool -> Maybe String -> Package
                 -> AnyBranch -> IO ()
updateSourcesPkg force allowHEAD distgit mver pkg br = do
  when (distgit && br /= RelBranch Rawhide && isRelBranch br) $
    promptEnter $ "Are you sure you want to update" +-+ show br +-+ "branch?! Press Enter to continue"
  spec <- if allowHEAD
          then findSpecfile
          else localBranchSpecFile pkg br
  curver <- pkgVersion spec
  diff <- filter (not . ("@@ " `isPrefixOf`)) <$> gitLines "diff" ["-U0", "HEAD", spec]
  let vdiff = filter (\v -> any (`isPrefixOf` v) ["-Version:","+Version:"]) diff
      mnewrel = find ("+Release:" `isPrefixOf`) diff
  when (length vdiff > 2) $
    error' $ "diff contains complex multi-version changes:\n" ++ unlines vdiff
  case mver of
    Nothing -> do
      putStrLn $ "current version:" +-+ curver
    Just nver -> do
      when (length vdiff == 2) $
        error' $ "spec version already bumped to" +-+ curver
      when (curver == nver) $
        putStrLn $ "already new version" +-+ curver
  let moldnewver =
        case mver of
          Just nver -> Just (curver,nver)
          Nothing ->
            case map extractFieldValue vdiff of
              [old,new] -> Just (old,new)
              _ -> Nothing
  when (isJust mver) $
    when (isJust moldnewver) $ do
    let (oldver,newver) =
          fromMaybe (error' "complex version change") moldnewver
    -- FIXME take epoch into account
    when (rpmVerCompare oldver newver == GT) $
      putStrLn $ "current" +-+ oldver +-+ "is newer!"
    putStrLn $ oldver +-+ "->\n" ++ newver
    when (curver /= newver) $ do
      specversions <- grep "^Version:" spec
      case specversions of
        [] -> error' "no Version field found"
        [specver] ->
          when ('%' `elem` specver) $
          error' "Please edit complex 'Version' fields with macro(s) by hand"
        _ -> error' "Please edit package with multiple versions by hand"
      editSpecField "Version" newver spec
      autorelease <- isAutoRelease spec
      if autorelease
        then do
        autoreset <- autoReleaseReset spec
        when autoreset $
          editSpecField "Release" "%autorelease" spec
        -- FIXME if multiple versions need to bump release
        else editSpecField "Release" "0%{?dist}" spec
      -- FIXME should be sure sources exists for distgit
      whenM (doesFileExist "sources") $
        cmd_ "sed" ["-i", "/" ++ unPackage pkg ++ "-" ++ oldver ++ "./d", "sources"]
  when distgit $ do
    -- FIXME forM_
    sources <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
    existing <- filterM doesFileExist sources
    unless (existing == sources) $ do
      fedpkg_ "sources" []
      unless force $
        -- FIXME only if not all exist
        cmd_ "spectool" ["-g", "-S", spec]
    patches <- map sourceFieldFile <$> cmdLines "spectool" ["-P", spec]
    forM_ patches $ \patch -> do
      unlessM (doesFileExist patch) $
        cmd_ "spectool" ["-g", "-P", spec]
      git_ "add" [patch]
    let (archives,textsources) = partition isArchiveFile sources
    when force $ do
      forM_ archives removeFile
      cmd_ "spectool" ["-g", "-S", spec]
    krbTicket
    fedpkg_ "new-sources" archives
    unless (null textsources) $
      git_ "add" textsources
  whenJust moldnewver $ \(_old,newver) -> do
    whenM (yesNoDefault False "Do you want to diff the sources") $
      compareTarballsCmd Nothing Nothing Nothing
    versions <- changelogVersions spec
    let missing =
          case versions of
            [] -> True
            (h:_) -> not $ (newver ++ "-") `isPrefixOf` h
    when missing $ do
      -- FIXME newver may contain macros!
      newversion <- pkgVersion spec
      cmd_ "rpmdev-bumpspec" ["-c", "Update to" +-+ newversion, spec]
      newrelease <- cmd "rpmspec" ["-q", "--srpm", "--undefine=dist", "--qf", "%{release}", spec]
      -- revert release if was already bumped
      whenJust (extractFieldValue <$> mnewrel) $ \newrel -> do
        editSpecField "Release" newrel spec
        cmd_ "sed" ["-i", "s/> -" +-+ newversion ++ '-' : newrelease ++ "/> -" +-+ newversion ++ '-' : (replace "%{?dist}" "" newrel) ++ "/" , spec]
      git_ "commit" ["-a", "-m", "Update to" +-+ newversion]
  putStr "Prepping... "
  sourcediropt <- sourceDirCwdOpt
  withTempDir $ \tempdir -> do
    cwd <- getCurrentDirectory
    withCurrentDirectory tempdir $ do
      cmdSilent' "rpmbuild" $ "-bp" : sourcediropt ++ ["--nodeps", cwd </> spec]
      putStrLn "done"
  -- FIXME git amend (if previous commit was update)

changelogVersions :: FilePath -> IO [String]
changelogVersions spec = do
  ns <- cmdLines "rpmspec" ["-q", "--srpm", "--qf", "%{changelogname}", spec]
  return $ map (removePrefix "- " . dropWhile (/= '-')) ns

-- eg "?Release:    0.2%{?dist}"
extractFieldValue :: String -> String
extractFieldValue vs =
  case headMay . tailSafe . words $ vs of
    Just h -> h
    Nothing -> error' $ "no field value:" +-+ vs
