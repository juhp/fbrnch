module Cmd.CompareTarballs (
  compareTarballsCmd
  )
where

import SimpleCmd
import SimpleCmd.Git
import System.IO.Extra (withTempDir)

import Common
import Common.System
import Patch (simplifyMinimalDiff)

compareTarballsCmd :: IO ()
compareTarballsCmd = do
  let sourcesfile = "sources"
  havesrc <- doesFileExist sourcesfile
  unless havesrc $ error' "no sources file found"
  diff <- git "diff" ["-U0", "origin", sourcesfile]
  case lines diff of
    [] -> error' $ "no uncommitted version changes in" +-+ show sourcesfile
    ls ->
      case simplifyMinimalDiff ls of
        [s1,s2] -> diffTarballs s1 s2
        _ -> error' $ "could not determine changed versions in" +-+ show sourcesfile
  where
    diffTarballs s1 s2 = do
      let src1 = getSrcFilename s1
          src2 = getSrcFilename s2
      cwd <- getCurrentDirectory
      when (s1 == s2) $
        warning $ "same filenames:" +-+ s1
      withTempDir $ \tmp ->
        withCurrentDirectory tmp $ do
        adir <- getDirTop "a" cwd src1
        bdir <- getDirTop "b" cwd src2
          -- createDirectory "b"
          -- withCurrentDirectory "b" $
          --   cmd_ "tar" ["xf", cwd </> src2]
          --   listDirectory "."
        cmd_ "diff" ["-u", "-r", "-w", "a" </> adir, "b" </> bdir]

    getSrcFilename s =
      -- "+SHA512 (filestore-0.6.5.1.tar.gz) = 9eec4d23737bcdfc3d0ded..."
      case words s of
        (_:"SHA512") : ('(':fp) : "=" : _hash -> init fp
        _ -> error' $ "Failed to parse:" +-+ s

    getDirTop :: FilePath -> FilePath -> FilePath -> IO FilePath
    getDirTop subdir topdir src = do
      createDirectory subdir
      withCurrentDirectory subdir $ do
        cmd_ "tar" ["xf", topdir </> src]
        ls <- listDirectory "."
        case ls of
          [n] -> return n
          _ -> error' $ "more than one top-level dir in" +-+ src
