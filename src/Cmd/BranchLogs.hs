module Cmd.BranchLogs (
  branchLogCmd
  )
where

import Control.Monad (forM_, when)
import Data.Function (on)
import Data.List.Extra (dropPrefix, {-groupOn, groupSortOn, isSuffixOf, sortOn,-} splitOn,
                        stripInfix, {-takeWhileEnd,-} uncons)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Distribution.Fedora.Branch (getActiveBranches, readBranch)
import System.Console.Pretty (color, Color(..), supportsPretty)

import Branches
import Common.System
import Package

-- FIXME handle missing local branch
-- FIXME --all-commits, --all-branches
-- FIXME find latest branches (local and remote)
-- FIXME how to handle dirty
-- FIXME handle detached head
branchLogCmd :: Bool -> Bool -> (BranchesReq,[String]) -> IO ()
branchLogCmd latest nosimplydecor (breq, pkgs) = do
  colored <- supportsPretty
  active <- getActiveBranches
  if null pkgs
    then logPkg colored active "."
    else mapM_ (logPkg colored active) pkgs
--  withPackagesMaybeBranch (if length pkgs > 1 then HeaderMust else HeaderNone) False Nothing (logPkg colored active) (breq, packages)
 where
   logPkg :: Bool -> [Branch] -> FilePath -> IO ()
   logPkg colored active path =
     withExistingDirectory path $ do
       branches <- listOfBranches True True breq
       if latest
         then aheadBranches branches
         else do
         forM_ branches $ \br -> do
           when (length pkgs > 1 || length branches > 1) $ do
             pkg <- getPackageName "."
             if length branches > 1
               then putPkgBrnchHdr pkg br
               else putPkgHdr pkg
           commits <- getLogCommits simplydecor active br
           mapM_ (putLogCommit colored) commits
     where
       simplydecor = not nosimplydecor

       aheadBranches :: [Branch] -> IO ()
       aheadBranches branches = do
         logbrs <- fmap (NE.toList . NE.nubBy ((==) `on` NE.head)) <$> mapM (getLogCommits True active) $ NE.fromList branches
         let containedIn l = any ((NE.head l `elem`) . NE.tail)
             reduced = [ l | l <- logbrs, not (l `containedIn` logbrs)]
         forM_ reduced $ \r -> do
           mapM_ (putLogCommit colored) r
           putChar '\n'

data BranchRemote = BR (Maybe String) Branch
  deriving Eq

instance Ord BranchRemote where
  compare (BR mr1 b1) (BR mr2 b2) =
    compare (b1,mr1) (b2,mr2)

type BranchList = NonEmpty BranchRemote

data LogCommit = LogCommit
                 { logRef :: String,
                   logBranches :: BranchList,
                   _logText :: String,
                   _logDate :: String }

instance Eq LogCommit where
  lc1 == lc2 = logRef lc1 == logRef lc2

instance Ord LogCommit where
  lc1 <= lc2 = NE.head (logBranches lc1) <= NE.head (logBranches lc2)

logCommitFormat :: String
logCommitFormat = "--pretty=format:%h\US%D\US%s\US%ch"

getLogCommits :: Bool -> [Branch] -> Branch -> IO (NonEmpty LogCommit)
getLogCommits simplydecor active br = do
   ls <- mapMaybe readLogCommit <$> gitLines "log" (["--simplify-by-decoration" | simplydecor] ++ [logCommitFormat, showBranch br])
   case NE.nonEmpty ls of
     Nothing -> error' $ "empty branch:" +-+ showBranch br
     Just ne -> return ne
  where
    readLogCommit :: String -> Maybe LogCommit
    readLogCommit cs =
      case splitOn "\US" cs of
        [h,ds,s,t] ->
          let mbrs =
                NE.nonEmpty $
                filter (\b' -> any (hasBranch b') active) $
                mapMaybe readBranchRemote $
                map (dropPrefix "HEAD -> ") $ splitOn ", " ds
          in
            case mbrs of
              Nothing -> Nothing
              Just brs -> Just $ LogCommit h (NE.sort brs) s t
        _ -> error' $ "malformed log line:" +-+ cs

hasBranch :: BranchRemote -> Branch -> Bool
hasBranch (BR _ b1) b2 = b1 == b2

readBranchRemote :: String -> Maybe BranchRemote
readBranchRemote s =
  case stripInfix "/" s of
    Nothing ->
      case readBranch s of
        Nothing -> Nothing
        Just br -> Just $ BR Nothing br
    Just (r,b) ->
      case readBranch b of
        Nothing -> Nothing
        Just br -> Just $ BR (Just r) br

showBR :: BranchRemote -> String
showBR (BR mr b) =
  case mr of
    Nothing -> showBranch b
    Just r -> r ++ '/' : showBranch b

showLogCommit :: Bool -> LogCommit -> String
showLogCommit colored (LogCommit h brs s time) =
  h +-+ showBranches colored brs +-+ s +-+ '(' : time ++ ")"

putLogCommit :: Bool -> LogCommit -> IO ()
putLogCommit colored = putStrLn . showLogCommit colored

showBranches :: Bool -> BranchList -> String
showBranches colored =
  unwords . reverse . NE.toList . NE.map renderBranches . NE.groupWith1 toBranch
  where
    toBranch (BR _ b) = b

    -- groupSortWith1 :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
    -- groupSortWith1 f = map (map snd) . NE.groupAllWith1 fst . NE.sortWith fst . map (f &&& id)

    remote (BR mr _) = mr

    renderBranches bs@(b :| t) =
          case uncons t of
            Nothing -> docolor Red $ showBR b
            Just (b',t') ->
              if null t'
              then
                case (remote b, remote b') of
                  (Nothing, Just "origin") ->
                    origin ++ docolor Green (showBranch $ toBranch b)
                  _ -> unwords $ map (docolor Red . showBR) $ NE.toList bs
              else unwords $ map (docolor Magenta . showBR) $ NE.toList bs
      where
        origin = if colored then "origin/" else "{origin/}"

        docolor c = if colored then color c else id
