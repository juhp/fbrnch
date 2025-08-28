module Cmd.CompareTarballs (
  compareTarballsCmd
  )
where

import Safe (tailSafe)
import SimpleCmd
import SimpleCmd.Git
import System.IO.Extra (withTempDir)

import Common
import Common.System
import Package (findSpecfile, pkgVersion)
import Patch (simplifyMinimalDiff)

compareTarballsCmd :: Maybe String -> Maybe String -> IO ()
compareTarballsCmd (Just nv1) (Just nv2) = diffTarballs nv1 nv2
compareTarballsCmd (Just nv) Nothing = do
  spec <- findSpecfile
  cur <- pkgVersion spec
  if nv == cur
    then error' "same source version!"
    else diffTarballs nv cur
compareTarballsCmd Nothing (Just _) = error' "impossible happened: only second arg"
compareTarballsCmd Nothing Nothing = do
  let sourcesfile = "sources"
  havesrc <- doesFileExist sourcesfile
  if havesrc
    then do
    diff <- git "diff" ["-U0", "origin", sourcesfile]
    case lines diff of
      [] -> error' $ "no uncommitted version changes in" +-+ show sourcesfile
      ls ->
        case simplifyMinimalDiff ls of
          [s1,s2] -> diffTarballs (sourcesFilename s1) (sourcesFilename s2)
          _ -> error' $ "could not determine changed versions in" +-+ show sourcesfile
    else do
    spec <- findSpecfile
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
            diffTarballs (name ++ '-' : specVersion v1 <.> "tar.gz") newtar
          _ -> error' $ "could not determine changed versions in" +-+ show sourcesfile
  where
    sourcesFilename s =
      -- "+SHA512 (filestore-0.6.5.1.tar.gz) = 9eec4d23737bcdfc3d0ded..."
      case words s of
        (_:"SHA512") : ('(':fp) : "=" : _hash -> init fp
        _ -> error' $ "Failed to parse:" +-+ s

    specVersion s =
      -- "-Version:        0.1.50.1"
      case words s of
        (_:"Version:") : [ver] -> ver
        _ -> error' $ "Failed to parse:" +-+ s

-- FIXME --reverse diff
diffTarballs :: String -> String -> IO ()
diffTarballs src1 src2 = do
  cwd <- getCurrentDirectory
  withTempDir $ \tmp ->
    withCurrentDirectory tmp $ do
    adir <- getDirTop "a" cwd src1
    bdir <- getDirTop "b" cwd src2
      -- createDirectory "b"
      -- withCurrentDirectory "b" $
      --   cmd_ "tar" ["xf", cwd </> src2]
      --   listDirectory "."
    void $ cmdBool "diff" ["-u", "-r", "--color=auto", "-w", "a" </> adir, "b" </> bdir]
  where
    getDirTop :: FilePath -> FilePath -> FilePath -> IO FilePath
    getDirTop subdir topdir src = do
      createDirectory subdir
      withCurrentDirectory subdir $ do
        cmd_ "tar" ["xf", topdir </> src]
        ls <- listDirectory "."
        case ls of
          [n] -> return n
          _ -> error' $ "more than one top-level dir in" +-+ src
