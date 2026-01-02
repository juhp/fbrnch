module Cmd.BranchLogs (
  branchLogCmd
  )
where

import Control.Monad (forM_, unless, when)
import Data.Function (on)
import Data.List.Extra (dropPrefix, find, splitOn, stripInfix, uncons)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Distribution.Fedora.Branch (readBranch)
import System.Console.Pretty (color, Color(..), supportsPretty)

import Branches
import Common.System
import Git (gitSwitchBranch)
import Package

-- FIXME --all-commits
-- FIXME select latest branches (local and remote)
-- FIXME how to handle dirty
-- FIXME handle detached head
branchLogCmd :: Bool -> Bool -> Bool -> (BranchesReq,[String]) -> IO ()
branchLogCmd latest allbrs nosimplydecor (breq, pkgs) = do
  colored <- supportsPretty
  if null pkgs
    then logPkg colored "."
    else mapM_ (logPkg colored) pkgs
--  withPackagesMaybeBranch (if length pkgs > 1 then HeaderMust else HeaderNone) False Nothing (logPkg colored active) (breq, packages)
 where
   logPkg :: Bool -> FilePath -> IO ()
   logPkg colored path =
     withExistingDirectory path $ do
     -- FIXME handle inactive branch automatically (without --inactive or error)
     branches <- listOfBranches True (not allbrs) breq
     let listBranches' =
           if allbrs then listAllBranches else listBranches
     locals <- listBranches' True
     remotes <- listBranches' False
     current <- gitCurrentBranch
     forM_ branches $ \br ->
       unless (br `elem` locals) $
       if br `elem` remotes
       then
         gitSwitchBranch (RelBranch br)
       else error' $ "branch does not exist:" +-+ showBranch br
     gitSwitchBranch current
     if latest
       then latestBranches branches
       else do
       forM_ branches $ \br -> do
         when (length pkgs > 1 || length branches > 1) $ do
           pkg <- getPackageName "."
           if length branches > 1
             then putPkgBrnchHdr pkg br
             else putPkgHdr pkg
         commits <- getLogCommits allbrs simplydecor br
         checkBranchOrder br $ NE.tail commits
         mapM_ (putLogCommit colored) commits
     where
       simplydecor = not nosimplydecor

       checkBranchOrder br oldcommits = do
         let brs = map logBranches oldcommits
             newer = mapMaybe (find ((br<) . toBranch)) brs
         unless (null newer) $
           putStrLn $ showBranch br +-+ "is ahead of:" +-+ unwords (map showBR newer)

       latestBranches :: [Branch] -> IO ()
       latestBranches branches = do
         logbrs <- fmap (NE.toList . NE.nubBy ((==) `on` NE.head)) <$> mapM (getLogCommits allbrs True) $ NE.fromList branches
         let containedIn l = any ((NE.head l `elem`) . NE.tail)
             reduced = [ l | l <- logbrs, not (l `containedIn` logbrs)]
         forM_ reduced $ \r -> do
           checkBranchOrders r
           mapM_ (putLogCommit colored) r
           putChar '\n'

       checkBranchOrders commits = do
         let brs = NE.map logBranches commits
         forM_ (NE.tails1 brs) $ \(b:|bs) ->
           forM_ b $ \b' -> do
           let newer = mapMaybe (find (b'<)) bs
           unless (null newer || allbrs) $
             putStrLn $ showBR b' +-+ "is ahead of:" +-+ unwords (map showBR newer)

data BranchRemote = BR (Maybe String) Branch
  deriving Eq

toBranch :: BranchRemote -> Branch
toBranch (BR _ b) = b

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

getLogCommits :: Bool -> Bool -> Branch -> IO (NonEmpty LogCommit)
getLogCommits allbrs simplydecor br = do
  existing <- (if allbrs then listAllBranches else listBranches) True
  unless (br `elem` existing) $
    error' $ "no local branch:" +-+ showBranch br
  ls <- mapMaybe (readLogCommit existing) <$> gitLines "log" (["--simplify-by-decoration" | simplydecor] ++ [logCommitFormat, showBranch br])
  case NE.nonEmpty ls of
    Nothing -> error' $ "empty branch:" +-+ showBranch br
    Just ne -> return ne
  where
    readLogCommit :: [Branch] -> String -> Maybe LogCommit
    readLogCommit existing cs =
      case splitOn "\US" cs of
        [h,ds,s,t] ->
          let mbrs =
                NE.nonEmpty $
                filter (\b' -> any (hasBranch b') existing) $
                mapMaybe (readBranchRemote . dropPrefix "HEAD -> ") $
                splitOn ", " ds
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
