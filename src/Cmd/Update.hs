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

-- FIXME check EVR increased
-- FIXME if multiple sources might need to bump release
updateCmd :: Bool -> Bool -> Bool -> (Maybe Branch,[String]) -> IO ()
updateCmd onlysources force allowHEAD (mbr,args) = do
  pkgGit <- isPkgGitSshRepo
  let (mver,pkgs) = case args of
        [a] -> if pkgGit then (Just a,[]) else (Nothing,[a])
        _ -> (Nothing,args)
  withPackagesMaybeBranch (Just False) (if allowHEAD then dirtyGitHEAD else dirtyGitFetch) ZeroOrOne (updatePkg mver) (mbr, pkgs)
  where
    updatePkg :: Maybe String -> Package -> AnyBranch -> IO ()
    updatePkg mver pkg br = do
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
      unless onlysources $ do
        let (oldver,newver) =
              case mver of
                Just nver -> (curver,nver)
                Nothing ->
                  case map (last . words) vdiff of
                    [old,new] -> (old,new)
                    _ -> error' "complex version change"
        -- FIXME take epoch into account
        when (rpmVerCompare oldver newver == GT) $
          putStrLn $ "current" +-+ oldver +-+ "is newer!"
        putStrLn $ oldver ++ " ->\n" ++ newver
        if curver /= newver
          then do
          editSpecField "Version" newver spec
          editSpecField "Release" "0%{?dist}" spec
          cmd_ "rpmdev-bumpspec" ["-c", "update to " ++ newver, spec]
          cmd_ "sed" ["-i", "/" ++ unPackage pkg ++ "-" ++ oldver ++ "./d", "sources"]
          else do
          versions <- changelogVersions spec
          let missing = null versions || not ((newver ++ "-") `isPrefixOf` head versions)
          when missing $ do
            cmd_ "rpmdev-bumpspec" ["-c", "update to " ++ newver, spec]
            -- FIXME need to commit after add sources
            git_ "commit" ["-a", "-m", "update to " ++ newver]
      whenM isPkgGitSshRepo $ do
        -- FIXME forM_
        sources <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
        existing <- filterM doesFileExist sources
        unless (existing == sources) $ do
          cmd_ "fedpkg" ["sources"]
          -- FIXME only if not all exist
          cmd_ "spectool" ["-g", "-S", spec]
        when force $ do
          let archives = filter isArchiveFile existing
          forM_ archives removeFile
          cmd_ "spectool" ["-g", "-S", spec]
        krbTicket
        copyFile "sources" "sources.fbrnch"
        cmd_ "fedpkg" $ "new-sources" : filter isArchiveFile sources
        --shell_ $ "cat sources.fbrnch >>" +-+ "sources"
        removeFile "sources.fbrnch"
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

editSpecField :: String -> String -> FilePath -> IO ()
editSpecField field new spec =
  cmd_ "sed" ["-i", "-e s/^\\(" ++ field ++ ":\\s\\+\\).*/\\1" ++ new ++ "/", spec]
