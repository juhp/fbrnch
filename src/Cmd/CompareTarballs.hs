module Cmd.CompareTarballs (
  compareTarballsCmd
  )
where

import Safe (tailSafe)
import SimpleCmd.Git
import System.Console.Pretty (supportsPretty)
import System.IO.Extra (withTempDir)

import Common
import Common.System
import Package (findSpecfile, isArchiveFile, pkgVersion)
import Patch (simplifyMinimalDiff)

compareTarballsCmd :: Maybe Natural -> Maybe String -> Maybe String -> IO ()
compareTarballsCmd mmaxlen (Just nv1) (Just nv2) = do
  spec <- findSpecfile
  diffTarballs spec mmaxlen nv1 nv2
compareTarballsCmd mmaxlen (Just nv) Nothing = do
  spec <- findSpecfile
  cur <- pkgVersion spec
  if nv == cur
    then error' "same source version!"
    else diffTarballs spec mmaxlen nv cur
compareTarballsCmd _ Nothing (Just _) = error' "impossible happened: only second arg"
compareTarballsCmd mmaxlen Nothing Nothing = do
  spec <- findSpecfile
  let sourcesfile = "sources"
  havesrc <- doesFileExist sourcesfile
  if havesrc
    then do
    -- was "origin"
    diff <- git "diff" ["-U0", "--cached", sourcesfile]
    case lines diff of
      [] -> error' $ "no uncommitted version changes in" +-+ show sourcesfile
      ls ->
        -- FIXME cannot handle majorversion macros etc (eg lean4)
        case simplifyMinimalDiff ls of
          [s1@('-':_),s2@('+':_)] -> diffTarballs spec mmaxlen (sourcesFilename s1) (sourcesFilename s2)
          ss -> do
            putNewLn
            error' $
              unlines $
              ("More than one source archive changed in" +-+ show sourcesfile ++ ":") : map sourcesFilename ss ++ ["", "Run: fbrnch compare-tarballs <source1> <source2>"]
    else do
    diff <- git "diff" ["-U0", spec]
    case lines diff of
      [] -> error' $ "no uncommitted version changes in" +-+ show spec
      ls ->
        case filter (("Version:" `isPrefixOf`) . tailSafe) $ simplifyMinimalDiff ls of
          [v1,v2] -> do
            let name = takeBaseName spec
            let newtar = name ++ '-' : specVersion v2 <.> "tar.gz"
            exists <- doesFileExist newtar
            unless exists $ cmd_ "spectool" ["-g", "-S", spec]
            diffTarballs spec mmaxlen (name ++ '-' : specVersion v1 <.> "tar.gz") newtar
          _ -> error' $ "could not determine changed versions in" +-+ show sourcesfile
  where
    sourcesFilename s =
      -- "+SHA512 (filestore-0.6.5.1.tar.gz) = 9eec4d23737bcdfc3d0ded..."
      case words s of
        (_:"SHA512") : ('(':fp) : "=" : _hash -> init fp
        _ -> error $ "Failed to parse:" +-+ s

    specVersion s =
      -- "-Version:        0.1.50.1"
      case words s of
        (_:"Version:") : [ver] -> ver
        _ -> error $ "Failed to parse:" +-+ s

-- FIXME --reverse diff
diffTarballs :: FilePath -> Maybe Natural -> String -> String -> IO ()
diffTarballs spec mmaxlen src1 src2 = do
  cwd <- getCurrentDirectory
  withTempDir $ \tmp ->
    withCurrentDirectory tmp $ do
    adir <- getDirTop "a" cwd src1
    bdir <- getDirTop "b" cwd src2
      -- createDirectory "b"
      -- withCurrentDirectory "b" $
      --   cmd_ "tar" ["xf", cwd </> src2]
      --   listDirectory "."
    color <- supportsPretty
    pipe_
      -- FIXME add option to filter out certain files
      ("diff", ["--color=always" | color] ++ ["-u", "-r", "-w", "a" </> adir, "b" </> bdir])
      ("grep", ["-v", ".\\{" ++ maybe "200" show mmaxlen ++ ",\\}"])
  where
    getDirTop :: FilePath -> FilePath -> FilePath -> IO FilePath
    getDirTop subdir topdir src = do
      createDirectory subdir
      withCurrentDirectory subdir $ do
        let archive = topdir </> src
        unlessM (doesFileExist archive) $ do
          downloadMissingArchive topdir spec archive src
          unlessM (doesFileExist archive) $
            error' $ src +-+ "not found (Source URL may not use %{version})"
        cmd_ "tar" ["xf", archive]
        ls <- listDirectory "."
        case ls of
          [n] -> return n
          _ -> error' $ "more than one top-level dir in" +-+ src

-- Derive version from archive name (Name-Version.tar.gz) or bare version arg
versionFromSrc :: FilePath -> FilePath -> String
versionFromSrc spec src =
  let name = takeBaseName spec
      base = dropArchiveExt $ takeFileName src
      prefix = name ++ "-"
  in if prefix `isPrefixOf` base
     then drop (length prefix) base
     else src

dropArchiveExt :: FilePath -> FilePath
dropArchiveExt f
  | ".tar." `isInfixOf` f = dropExtension $ dropExtension f
  -- note also checks .tar. and .gpg
  | isArchiveFile f = dropExtension f
  | otherwise = f

-- spectool --define version is ignored (Version: wins), so rewrite the
-- expanded Source URL by substituting the current version with the needed one.
downloadMissingArchive :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
downloadMissingArchive topdir spec archive src = do
  let ver = versionFromSrc spec src
  curver <- pkgVersion $ topdir </> spec
  if ver == curver
    then cmd_ "spectool" ["-g", "-S", "-C", topdir, topdir </> spec]
    else do
      fields <- cmdLines "spectool" ["-S", topdir </> spec]
      let urls = map (last . words) fields
          currentName = replace ver curver $ takeFileName src
      case filter ((== currentName) . takeFileName) urls of
        [] ->
          error' $ "could not find Source URL for" +-+ currentName
        (url:_) ->
          let oldurl = replace curver ver url
          in cmd_ "curl"
               ["--fail", "--show-error", "--location",
                "--output", archive, oldurl]
