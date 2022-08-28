module Cmd.Merge (
  mergeCmd,
  mergeBranch,
  getNewerBranch)
where

import Control.Applicative ((<|>))

import Common
import Common.System

import Branches
import Git
import Package
import Prompt

mergeCmd :: Bool -> Maybe Branch -> (BranchesReq,[String]) -> IO ()
mergeCmd noprompt mfrom =
  withPackagesByBranches HeaderMay False cleanGitFetchActive AnyNumber runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    runMergeBranch _pkg (RelBranch br) = do
      exists <- gitSwitchBranch' br
      when exists $
        whenJustM (return mfrom <|> getNewerBranch br) $ \from -> do
          when (from == br) $
            error' "cannot merge branch to itself"
          gitMergeOrigin br
          unmerged <- mergeable from br
          mergeBranch False noprompt unmerged from br

-- FIXME maybe require local branch already here
mergeable :: Branch -> Branch -> IO (Bool,[String])
mergeable _ Rawhide = return (False,[])
mergeable from _ = do
  locals <- localBranches True
  gitMergeable (show from `notElem` locals) from

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> (Bool,[String]) -- (ancestor,unmerged)
            -> Branch -> Branch -> IO ()
mergeBranch _ _ _ _ Rawhide = return ()
mergeBranch _ _ (_,[]) _ _ = return ()
mergeBranch build noprompt (True, unmerged) from br = do
  isnewrepo <- initialPkgRepo
  unless (null unmerged) $ do
    putStrLn $ (if isnewrepo || noprompt then "Merging from" else "New commits in") ++ " " ++ show from ++ ":"
    mapM_ putStrLn unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ putStrLn unpushed
  -- FIXME avoid Mass_Rebuild bumps
  mmerge <-
    if isnewrepo && length unmerged == 1 || noprompt
    then return $ Just Nothing
    else refPrompt unmerged $ "Press Enter to merge " ++ show from ++
         (if build then " and build" else "") ++
         (if length unmerged > 1 then "; or give a ref to merge" else "") ++
         "; or 'no' to skip merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ mhash -> do
    let ref = case mhash of
                Nothing -> show from
                Just hash -> hash
    locals <- localBranches True
    unless (show from `elem` locals) $
      git_ "fetch" ["origin", show from ++ ":" ++ show from]
    git_ "merge" ["--quiet", ref]
mergeBranch build noprompt (False,unmerged) from br = do
  putStrLn $ show from ++ " branch is not directly mergeable:"
  mapM_ putStrLn unmerged
  putStrLn ""
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    mapM_ putStrLn unpushed
  mmerge <-
    if noprompt then return Nothing
    else conflictPrompt unmerged $ "Press Enter to skip merge" ++ (if build then " and build" else "") ++ "; or give a ref or 'HEAD' to attempt merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    git_ "merge" [ref]
