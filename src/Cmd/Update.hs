module Cmd.Update
  ( updateCmd
  )
where

import SimpleCmd

import Branches
import Common
import Common.System
import Git
import Krb
import Package

-- FIXME branch arg?
updateCmd :: [String] -> IO ()
updateCmd args = do
  pkgGit <- isPkgGitRepo
  let (mver,pkgs) =
        if pkgGit
        then (listToMaybe args, if length args > 1 then error' "cannot specify packages in a dist-git repo" else [])
        else (Nothing, args)
  withPackageByBranches (Just False) dirtyGitFetch Nothing True ZeroOrOne (updatePkg mver) pkgs
  where
    updatePkg :: Maybe String -> Package -> AnyBranch -> IO ()
    updatePkg mver pkg br = do
      spec <- localBranchSpecFile pkg br
      (curver,_) <- pkgVerRel spec
      vdiff <- filter ("Version:" `isInfixOf`) . filter (not . ("@@ " `isPrefixOf`)) <$> gitLines "diff" ["-U0", "HEAD", spec]
      when (isNothing mver) $ do
        when (null vdiff) $
          error' "please specify version or edit in spec file"
        unless (length vdiff == 2) $
          error' $ "diff contains complex version change:\n" ++ unlines vdiff
      if Just curver == mver
        then putStrLn $ "already new version " ++ curver
        else do
        --FIXME: compare rpm versions
        -- if newver < oldver
        --   then putStrLn $ "current" +-+ display oldver +-+ "is newer!"
        --   else do
        pkgGit <- isPkgGitRepo
        let (oldver,newver) =
              case mver of
                Just nver -> (curver,nver)
                Nothing ->
                  case map (last . words) vdiff of
                    [old,new] -> (old,new)
                    _ -> error' "complex version change"
        putStrLn $ oldver ++ " ->\n" ++ newver
        when (curver /= newver) $ do
          editSpecField "Version" newver spec
          editSpecField "Release" "0%{?dist}" spec
          cmd_ "rpmdev-bumpspec" ["-c", "update to " ++ newver, spec]
        when pkgGit $ do
          sources <- map sourceFieldFile <$> cmdLines "spectool" ["-S", spec]
          cmd_ "sed" ["-i", "/" ++ unPackage pkg ++ "-" ++ oldver ++ "./d", "sources"]
          allExist <- and <$> mapM doesFileExist sources
          unless allExist $ do
            cmd_ "fedpkg" ["sources"]
            cmd_ "spectool" ["-g", "-S", spec]
          krbTicket
          copyFile "sources" "sources.fbrnch"
          cmd_ "fedpkg" $ "new-sources" : filter (".tar." `isInfixOf`) sources
          shell_ $ "cat sources.fbrnch >>" +-+ "sources"
          removeFile "sources.fbrnch"
          prepPackage pkg br
          cmd_ "git" ["commit", "-a", "-m", "update to " ++ newver]

    sourceFieldFile :: String -> FilePath
    sourceFieldFile field =
      if null field then
        -- should be impossible
        error "empty source field!"
      else (takeFileName . last . words) field

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
