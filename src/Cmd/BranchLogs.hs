module Cmd.BranchLogs (
  branchLogsCmd
  )
where

import Control.Monad.Extra (unlessM)
import Data.List.Extra (dropPrefix, groupSortOn, isSuffixOf, splitOn, takeWhileEnd)
import Data.Maybe (mapMaybe)
import Distribution.Fedora.Branch (getActiveBranches, readBranch)
import System.Console.Pretty (color, Color(..))

import Branches
import Common.System
import Git
import Package

-- FIXME --all-commits, --all-branches
branchLogsCmd :: (Maybe Branch,[String]) -> IO ()
branchLogsCmd (mbr, pkgs) = do
  unlessM isPkgGitRepo $
    error' "not a git repo"
  active <- getActiveBranches
  withPackagesMaybeBranch HeaderMay False Nothing (branchLogPkg active) (mbr, pkgs)
 where
   branchLogPkg :: [Branch] -> Package -> AnyBranch -> IO ()
   branchLogPkg active _pkg _br = do
     commits <- mapMaybe (readLogCommit active) <$> gitLines "log" (["--simplify-by-decoration", "--pretty=format:%h\US%D\US%s\US%ch"] ++ maybe [] (pure . showBranch) mbr)
     mapM_ displayLogCommit commits

data LogCommit = LogCommit
                 { _logRef :: String,
                   _logBranches :: [String],
                   _logText :: String,
                   _logDate :: String }
  deriving Show

readLogCommit :: [Branch] -> String -> Maybe LogCommit
readLogCommit active cs =
  case splitOn "\US" cs of
    [h,ds,s,t] ->
      let brs = filter (\d -> any (\b -> showBranch b `isSuffixOf` d) active) $ map (dropPrefix "HEAD -> ") $ splitOn ", " ds
      in if null brs
      then Nothing
      else Just $ LogCommit h brs s t
    _ -> error' $ "malformed log line:" +-+ cs

showLogCommit :: LogCommit -> String
showLogCommit (LogCommit h brs s t) =
  h +-+ showBranches brs +-+ s +-+ t
  where
    showBranches = unwords . map renderBranches . groupSortOn toBranch

    renderBranches bs =
      let col = if length bs > 1 then Green else Red
      in unwords $ map (color col) bs

    -- renderBranch br =
    --   if "origin/" `isPrefixOf` br
    --   then color Magenta br
    --   else color Green br

    toBranch = readBranch . takeWhileEnd (\c -> c /= '/' && c /= ' ')

displayLogCommit :: LogCommit -> IO ()
displayLogCommit =
  putStrLn . showLogCommit
