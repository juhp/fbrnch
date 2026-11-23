module Cmd.CompareTarballs (
  compareTarballsCmd,
  Src(..),
  readSrc
  )
where

import Data.Char (isDigit)
import Safe (headMay, tailSafe)
import SimpleCmd.Git
import System.Console.Pretty (supportsPretty)
import System.IO.Extra (withTempDir)

import Common
import Common.System
import Package (findSpecfile, isArchiveFile, pkgVersion, sourceFieldFile)
import Patch (simplifyMinimalDiff)

data Src = SrcVer String | SrcNameVer String | SrcArchive String

readSrc :: String -> Src
readSrc s =
  if isArchiveFile s
  then SrcArchive s
  else
    case headMay s of
      Just h | isDigit h -> SrcVer s
      _ -> SrcNameVer s

srcToArchive :: FilePath -> Src -> IO FilePath
srcToArchive spec src =
  case src of
    SrcArchive f -> return f
    SrcNameVer nv -> do
      let (n,v) = breakEnd (== '-') $ takeFileName nv
      cur <- pkgVersion spec
      sources <- filter isArchiveFile . map sourceFieldFile <$>
                 cmdLines "spectool" ["-S", spec]
      case filter (n `isPrefixOf`) $ map takeFileName sources of
        [] -> error' $ "unknown source" +-+ n
        [f] -> return $ replace cur v f
        _ -> do
          let f = nv <.> "tar.gz"
          putStrLn $ "trying with" +-+ f
          return f
    SrcVer v -> do
      sources <- filter isArchiveFile . map sourceFieldFile <$>
                 cmdLines "spectool" ["-S", spec]
      case sources of
        [] -> error' $ "unknown source for" +-+ v
        [f] -> do
          cur <- pkgVersion spec
          return $ replace cur v $ takeFileName f
        _ -> do
          warning "multiple source files"
          pkg <- getDirectoryName
          putStrLn $ "trying with" +-+ pkg
          return $ pkg ++ '-' : v <.> "tar.gz"

compareTarballsCmd :: Maybe Natural -> Maybe Src -> Maybe Src -> IO ()
compareTarballsCmd mmaxlen (Just src1) (Just src2) = do
  spec <- findSpecfile
  nv1 <- srcToArchive spec src1
  nv2 <- srcToArchive spec src2
  diffTarballs spec mmaxlen nv1 nv2
compareTarballsCmd mmaxlen (Just src) Nothing = do
  spec <- findSpecfile
  nv <- srcToArchive spec src
  cur <- pkgVersion spec
  nv' <- srcToArchive spec $ SrcVer cur
  if nv == nv'
    then error' "same source/version!"
    else diffTarballs spec mmaxlen nv nv'
compareTarballsCmd _ Nothing (Just _) = error' "impossible happened: only second arg"
compareTarballsCmd mmaxlen Nothing Nothing = do
  spec <- findSpecfile
  let sourcesfile = "sources"
  havesrc <- doesFileExist sourcesfile
  if havesrc
    then do
    -- was "origin"
    diff <- do
      ds <- git "diff" ["-U0", "--cached", sourcesfile]
      if null ds
        then git "diff" ["-U0", "HEAD^!", sourcesfile]
        else return ds
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
    -- print (src1,src2)
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
      -- filter out very long lines (often machine generated/formatted)
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
