module Cmd.BranchLogs (
  branchLogsCmd
  )
where

import Control.Monad.Extra (unlessM)
import Data.List.Extra (dropPrefix, groupSortOn, isSuffixOf, sortOn, splitOn,
                        stripInfix, takeWhileEnd, uncons)
import Data.Maybe (mapMaybe)
import Distribution.Fedora.Branch (getActiveBranches, readBranch)
import System.Console.Pretty (color, Color(..), supportsPretty)

import Branches
import Common.System
import Git
import Package

-- FIXME --all-commits, --all-branches
branchLogsCmd :: (Maybe Branch,[String]) -> IO ()
branchLogsCmd (mbr, pkgs) = do
  colored <- supportsPretty
  unlessM isPkgGitRepo $
    error' "not a git repo"
  active <- getActiveBranches
  withPackagesMaybeBranch HeaderMay False Nothing (branchLogPkg colored active) (mbr, pkgs)
 where
   branchLogPkg :: Bool -> [Branch] -> Package -> AnyBranch -> IO ()
   branchLogPkg colored active _pkg _br = do
     commits <- mapMaybe (readLogCommit active) <$> gitLines "log" (["--simplify-by-decoration", "--pretty=format:%h\US%D\US%s\US%ch"] ++ maybe [] (pure . showBranch) mbr)
     mapM_ (putStrLn . showLogCommit colored) commits

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

showLogCommit :: Bool -> LogCommit -> String
showLogCommit colored (LogCommit h brs s time) =
  h +-+ showBranches brs +-+ s +-+ time
  where
    showBranches :: [String] -> String
    showBranches =
      unwords . reverse . map (renderBranches . sortOn remote) . groupSortOn toBranch
      where
        toBranch = readBranch . takeWhileEnd (/= '/')

    remote br =
      case stripInfix "/" br of
        Nothing -> Nothing
        Just (r,_) -> Just r

    renderBranches bs =
      case uncons bs of
        Nothing -> ""
        Just (b,t) ->
          case uncons t of
            Nothing -> docolor Red b
            Just (b',t') ->
              if null t'
              then
                case (remote b, remote b') of
                  (Nothing, Just "origin") ->
                    origin ++ docolor Green b
                  _ -> unwords $ map (docolor Red) bs
              else unwords $ map (docolor Magenta) bs
      where
        origin = if colored then "origin/" else "{origin/}"

        docolor c = if colored then color c else id
