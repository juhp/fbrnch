module Cmd.Commit
  ( commitPkgs,
  )
where

import Common
import Common.System
import Git
import Package
import Prompt

-- FIXME use branches after all?
commitPkgs :: Maybe CommitOpt -> [String] -> IO ()
commitPkgs mopt args =
  if null args
    then commitPkg "."
    else mapM_ commitPkg args
  where
    commitPkg :: FilePath -> IO ()
    commitPkg path =
      withExistingDirectory path $
        unlessM isGitDirClean $ do
          getPackageName path >>= putPkgHdr
          opts <- case mopt of
            Just opt -> return $
              case opt of
                CommitMsg msg -> ["-m", msg]
                -- FIXME reject amend if already pushed
                CommitAmend -> ["--amend", "--no-edit"]
            Nothing -> do
              changelog <- do
                spec <- findSpecfile
                clog <- cleanChangelog <$> cmd "rpmspec" ["-q", "--srpm", "--qf", "%{changelogtext}", spec]
                when (length (lines clog) > 1) $
                  putStrLn clog
                diff <- git "diff" ["-U0", "HEAD"]
                if ("+- " ++ clog) `elem` lines diff
                  then putStrLn clog >> return clog
                  else putStrLn diff >> readCommitMsg
              return ["-m", changelog]
          git_ "commit" $ "-a" : opts

readCommitMsg :: IO String
readCommitMsg = do
  clog <- prompt "\nPlease input the commit message"
  if null clog
    then readCommitMsg
    else return clog
