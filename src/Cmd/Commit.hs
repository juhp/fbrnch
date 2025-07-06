module Cmd.Commit
  ( commitCmd,
  )
where

import SimplePrompt (promptEnter, promptNonEmpty)

import Branches (getReleaseBranch, showBranch, Branch(Rawhide))
import Common
import Common.System
import Git
import Package

-- FIXME reject if nvr ahead of newer branch
-- FIXME use branches after all?
-- FIXME handle multiline changelog entries with "-m description"
-- FIXME --undo last change: eg undo accidential --amend
-- FIXME --empty
-- FIXME include only (used) changelog if not staged
commitCmd :: Bool -> Bool -> Maybe CommitOpt -> Bool -> Bool -> [String]
          -> IO ()
commitCmd dryrun branched mopt firstLine unstaged paths = do
  distgit <- isPkgGitSshRepo
  if distgit
    then do
    br <- getReleaseBranch
    if br == Rawhide
      then when branched $
           error' "cannot use --branched for rawhide"
      else unless branched $
           promptEnter $ "Are you sure you want to update" +-+ showBranch br +-+ "branch?! Press Enter to continue"
    else when branched $
         error' "cannot use --branched with dist-git"
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
            if null paths && not unstaged
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
                autochangelog <- grep_ "^%autochangelog" spec
                if autochangelog
                  -- rpmautospec generates "Uncommitted changes"
                  then error' "set commit msg with --message"
                  else do
                  clog <- lines <$> cleanChangelog True spec
                  case clog of
                    [] -> readCommitMsg
                    [msg] -> return msg
                    (msg:_) ->
                      if firstLine
                      then return $ removePrefix "- " msg
                      else do
                        diff <- git "diff" ["-U0", if addall then "HEAD" else "--cached"]
                        let newlogs =
                              filter (\c -> ('+' : c) `elem` lines (unquoteMacros diff)) clog
                        case newlogs of
                          [] -> putStrLn diff >> readCommitMsg
                          [m] -> return (removePrefix "- " m)
                          [m,m'] -> mapM_ putStrLn newlogs >>
                                    return (unlines $ map (removePrefix "- ") [m,"",m'])
                          (m:ms) -> mapM_ putStrLn newlogs >>
                                    return (unlines (removePrefix "- " m:"":ms))
              return ["-m", changelog]
          if dryrun
            then cmdN "git" $ ["-a" | addall] ++ opts
            else git_ "commit" $ ["--dry-run" | dryrun] ++ ["-a" | addall] ++ opts

readCommitMsg :: IO String
readCommitMsg = do
  tty <- isTty
  if tty
    then promptNonEmpty "\nPlease input the commit message"
    else error' "please input commit message in a terminal"

unquoteMacros :: String -> String
unquoteMacros [] = []
unquoteMacros ('%':'%':cs) = '%':unquoteMacros cs
unquoteMacros (c:cs) = c : unquoteMacros cs
