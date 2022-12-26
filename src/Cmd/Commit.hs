module Cmd.Commit
  ( commitPkgs,
  )
where

import Common
import Common.System
import Git
import Package
import Prompt

-- FIXME reject if nvr ahead of newer branch
-- FIXME use branches after all?
-- FIXME handle multiline changelog entries with "-m description"
-- FIXME --undo last change: eg undo accidential --amend
-- FIXME for single package assume --all if no stage
commitPkgs :: Maybe CommitOpt -> Bool -> Bool -> [String] -> IO ()
commitPkgs mopt firstLine unstaged paths = do
  when (isJust mopt && firstLine) $
    error' "--first-line cannot be used with other commit msg options"
  if null paths
    then commitPkg "."
    else mapM_ commitPkg paths
  where
    commitPkg :: FilePath -> IO ()
    commitPkg dir =
      withExistingDirectory dir $
        unlessM isGitDirClean $ do
          getPackageName dir >>= putPkgHdr
          addall <-
            if null paths
            then null <$> git "diff" ["--cached"]
            else return unstaged
          opts <- case mopt of
            Just opt -> return $
              case opt of
                CommitMsg msg -> ["-m", msg]
                -- FIXME reject amend if already pushed
                CommitAmend -> ["--amend", "--no-edit"]
            Nothing -> do
              changelog <- do
                spec <- findSpecfile
                clog <- lines <$> cleanChangelog spec
                case clog of
                  [] -> readCommitMsg
                  [msg] -> putStrLn msg >> return msg
                  msgs ->
                    if firstLine
                    then return $ removePrefix "- " $ head msgs
                    else do
                      diff <- git "diff" ["-U0", if addall then "HEAD" else "--cached"]
                      let newlogs =
                            filter (\c -> ('+' : c) `elem` lines diff) clog
                      case newlogs of
                        [] -> putStrLn diff >> readCommitMsg
                        [msg] -> putStrLn msg >>
                                 return (removePrefix "- " msg)
                        (m:ms) -> mapM_ putStrLn newlogs >>
                                  return (unlines (removePrefix "- " m:"":ms))
              return ["-m", changelog]
          git_ "commit" $ ["-a" | addall] ++ opts

readCommitMsg :: IO String
readCommitMsg = do
  tty <- isTty
  if tty
    then do
    clog <- prompt "\nPlease input the commit message"
    if null clog
      then readCommitMsg
      else return clog
    else error' "please input commit message in a terminal"
