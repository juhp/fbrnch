module Cmd.Merge (
  mergeCmd,
  mergeBranch,
  getNewerBranch)
where

import Numeric.Natural (Natural)

import Common
import Common.System

import Branches
import Git
import Package
import Patch

mergeCmd :: Bool -> Bool -> Maybe Natural -> Bool -> Maybe Branch
         -> (BranchesReq,[String]) -> IO ()
mergeCmd dryrun noprompt mnotrivial showall mfrom =
  withPackagesByBranches HeaderMay False cleanGitFetchActive AnyNumber runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    runMergeBranch _pkg (RelBranch br) = do
      exists <- gitSwitchBranch' False br
      when exists $ do
        mfrom' <- if isJust mfrom
                  then return mfrom
                  else getNewerBranch br
        whenJust mfrom' $ \from -> do
          when (from == br) $
            error' "cannot merge branch to itself"
          unless dryrun $
            gitMergeOrigin br
          (ancestor,unmerged) <- mergeable from br
          unmerged' <- filterOutTrivial mnotrivial unmerged
          mergeBranch dryrun False noprompt showall (ancestor,unmerged') from br
      where
        filterOutTrivial :: Maybe Natural -> [Commit] -> IO [Commit]
        filterOutTrivial Nothing cs = return cs
        filterOutTrivial _ [] = return []
        filterOutTrivial (Just no) css@(c:cs) =
          if no == 0
          then return css
          else do
            -- drop oneline
            ls <- tail <$> gitLines "show" ["-U1", "--pretty=oneline", commitRef c]
            if isTrivialRebuildCommit ls
              then filterOutTrivial (Just (no -1)) cs
              else return css

-- FIXME maybe require local branch already here
mergeable :: Branch -> Branch -> IO (Bool,[Commit])
mergeable _ Rawhide = return (False,[])
mergeable from _ = do
  locals <- localBranches True
  gitMergeable (show from `notElem` locals) from

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> Bool -> Bool
            -> (Bool,[Commit]) -- (ancestor,unmerged)
            -> Branch -> Branch -> IO ()
mergeBranch _ _ _ _ _ _ Rawhide = return ()
mergeBranch _ _ _ _ (_,[]) _ _ = return ()
mergeBranch dryrun build noprompt showall (True, unmerged) from br = do
  isnewrepo <- initialPkgRepo
  putStrLn $ (if isnewrepo || noprompt then "Merging from" else "New commits in") ++ " " ++ show from ++ ":"
  displayCommits showall unmerged
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    displayCommits showall unpushed
  mmerge <-
    if isnewrepo && length unmerged == 1 || noprompt
    then return $ Just $ commitRef (head unmerged)
    else refPrompt unmerged ("Press Enter to merge " ++ show from ++
         (if build then " and build" else "") ++
         (if length unmerged > 1 then "; or give a ref to merge" else "") ++
         "; or 'no' to skip merge")
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref -> do
    locals <- localBranches True
    unless (show from `elem` locals) $
      git_ "fetch" ["origin", show from ++ ":" ++ show from]
    unless dryrun $
      git_ "merge" ["--quiet", ref]
mergeBranch dryrun build noprompt showall (False,unmerged) from br = do
  putStrLn $ show from ++ " branch is not directly mergeable:"
  displayCommits False unmerged
  putNewLn
  unpushed <- gitShortLog $ "origin/" ++ show br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    displayCommits showall unpushed
  mmerge <-
    if noprompt then return Nothing
    else conflictPrompt unmerged $ "Press Enter to skip merge" ++ (if build then " and build" else "") ++ "; or give a ref or 'HEAD' to attempt merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    unless dryrun $
    git_ "merge" [ref]
