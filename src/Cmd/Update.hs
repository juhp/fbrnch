module Cmd.Update
  ( updateCmd
  )
where

import Branches
import Common
import Common.System
import Git
import InterleaveOutput (cmdSilent')
import Krb
import Package

import Data.RPM.VerCmp
import SimplePrompt (prompt_)

-- FIXME don't bump release if already bumped
-- FIXME check EVR increased
-- FIXME if multiple sources might need to bump release
-- FIXME Haskell subpackages require release bump even with version bump
updateCmd :: Bool -> Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
updateCmd onlysources force allowHEAD (mbr,args) = do
  pkgGit <- isPkgGitSshRepo
  (mver,pkgs) <-
    case args of
      [a] -> do
        if pkgGit
          then return (Just a,[])
          else do
          mspec <- maybeFindSpecfile
          return $ if isJust mspec
                   then (Just a,[])
                   else (Nothing,[a])
      _ -> return (Nothing,args)
  let mgitops =
        let dirty = if allowHEAD then dirtyGitHEAD else dirtyGitFetch
        in if pkgGit
           then dirty
           else if null pkgs then Nothing else dirty
  withPackagesMaybeBranch HeaderMay False mgitops (updatePkg mver) (mbr, pkgs)
  where
    updatePkg :: Maybe String -> Package -> AnyBranch -> IO ()
    updatePkg mver pkg br = do
      when (br /= RelBranch Rawhide) $
        prompt_ $ "Are you sure you want to update " ++ show br ++ "?"
      spec <- if allowHEAD
              then findSpecfile
              else localBranchSpecFile pkg br
      (curver,_) <- pkgVerRel spec
      vdiff <- filter ("Version:" `isInfixOf`) . filter (not . ("@@ " `isPrefixOf`)) <$> gitLines "diff" ["-U0", "HEAD", spec]
      unless (length vdiff `elem` [0,2]) $
        error' $ "diff contains complex version change:\n" ++ unlines vdiff
      case mver of
        Nothing -> do
          when (null vdiff && not onlysources) $
            error' "specify or edit version to update"
          putStrLn $ "current version: " ++ curver
        Just nver -> do
          when (length vdiff == 2) $
            error' $ "spec version already bumped to " ++ curver
          when (curver == nver) $
            putStrLn $ "already new version " ++ curver
      let moldnewver =
            case mver of
              Just nver -> Just (curver,nver)
              Nothing ->
                case map (last . words) vdiff of
                  [old,new] -> Just (old,new)
                  _ -> Nothing
      unless onlysources $ do
        let (oldver,newver) =
              fromMaybe (error' "complex version change") moldnewver
        -- FIXME take epoch into account
        when (rpmVerCompare oldver newver == GT) $
          putStrLn $ "current" +-+ oldver +-+ "is newer!"
        putStrLn $ oldver ++ " ->\n" ++ newver
        when (curver /= newver) $ do
          editSpecField "Version" newver spec
          editSpecField "Release" "0%{?dist}" spec
          -- FIXME should be sure sources exists for distgit
          whenM (doesFileExist "sources") $
            cmd_ "sed" ["-i", "/" ++ unPackage pkg ++ "-" ++ oldver ++ "./d", "sources"]
      whenM isPkgGitSshRepo $ do
        -- FIXME forM_
        sources <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
        existing <- filterM doesFileExist sources
        unless (existing == sources) $ do
          cmd_ "fedpkg" ["sources"]
          unless force $
            -- FIXME only if not all exist
            cmd_ "spectool" ["-g", "-S", spec]
        patches <- cmdLines "spectool" ["-P", spec]
        forM_ patches $ \patch ->
          unlessM (doesFileExist patch) $
          cmd_ "spectool" ["-g", "-P", spec]
        when force $ do
          let archives = filter isArchiveFile existing
          forM_ archives removeFile
          cmd_ "spectool" ["-g", "-S", spec]
        krbTicket
        cmd_ "fedpkg" $ "new-sources" : filter isArchiveFile sources
      whenJust moldnewver $ \(_old,newver) -> do
        versions <- changelogVersions spec
        let missing = null versions || not ((newver ++ "-") `isPrefixOf` head versions)
        when missing $ do
          cmd_ "rpmdev-bumpspec" ["-c", "update to " ++ newver, spec]
          git_ "commit" ["-a", "-m", "update to " ++ newver]
      putStr "Prepping... "
      cmdSilent' "rpmbuild" ["-bp", spec]
      putStrLn "done"
      -- FIXME git amend (if previous commit was update)

    sourceFieldFile :: String -> FilePath
    sourceFieldFile field =
      if null field then
        -- should be impossible
        error "empty source field!"
      else (takeFileName . last . words) field

    -- FIXME handle .tgz?
    isArchiveFile :: FilePath -> Bool
    isArchiveFile f =
      ".tar." `isInfixOf` f || ".zip" `isSuffixOf` f

pkgVerRel :: FilePath -> IO (String,String)
pkgVerRel spec = do
  --dist <- branchDist br
  -- workaround dist with bootstrap
  --hostdist <- cmd "rpm" ["--eval", "%{dist}"]
  mvr <- cmdMaybe "rpmspec" ["-q", "--srpm", "--qf", "%{version}-%{release}", spec]
  case mvr of
    Nothing -> error' $ "Failed to read package ver-rel: " ++ spec
    Just vr -> return $ splitBy "-" vr

splitBy :: String -> String -> (String,String)
splitBy sep xs =
  let ws = splitOn sep xs in
    case ws of
      [f,v] -> (f,v)
      _ -> error $ "inconsistent field: " ++ xs

changelogVersions :: FilePath -> IO [String]
changelogVersions spec = do
  ns <- cmdLines "rpmspec" ["-q", "--srpm", "--qf", "%{changelogname}", spec]
  return $ map (removePrefix "- " . dropWhile (/= '-')) ns
