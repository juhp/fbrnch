module Cmd.Merge (
  mergeCmd,
  mergeBranch,
  getNewerBranch)
where

import Safe (tailSafe)
import SimplePrompt (promptEnter)
import System.Console.Pretty (color, Color(Magenta, Red))

import Common
import Common.System
import Branches
import Git
import Package
import Patch

-- FIXME add --local to merge from local branch instead of origin?
mergeCmd :: Bool -> Bool -> Bool -> Maybe Natural -> Bool -> Maybe Branch
         -> (BranchesReq,[String]) -> IO ()
mergeCmd dryrun nofetch noprompt mnotrivial showall mfrom =
  withPackagesByBranches HeaderMay False (if nofetch then cleanGitActive else cleanGitFetchActive) AnyNumber runMergeBranch
  where
    runMergeBranch :: Package -> AnyBranch -> IO ()
    runMergeBranch _ (OtherBranch _) =
      error' "merge only defined for release branches"
    -- FIXME should rawhide default to no-op
    runMergeBranch pkg (RelBranch br) = do
      when (mfrom == Just br) $
        error' "cannot merge branch to itself"
      exists <- gitSwitchBranch' False br
      when exists $ do
        unless dryrun $
          gitMergeOrigin br
        (mancestor,unmerged,from) <- mergeable pkg br mfrom
        unmerged' <- filterOutTrivial mnotrivial unmerged
        mergeBranch dryrun nofetch False noprompt showall pkg (mancestor,unmerged') from br
      where
        filterOutTrivial :: Maybe Natural -> [Commit] -> IO [Commit]
        filterOutTrivial Nothing cs = return cs
        filterOutTrivial _ [] = return []
        filterOutTrivial (Just no) css@(c:cs) =
          if no == 0
          then return css
          else do
            -- drop oneline
            ls <- tailSafe <$> gitLines "show" ["-U1", "--pretty=oneline", commitRef c]
            if isTrivialRebuildCommit ls
              then filterOutTrivial (Just (no -1)) cs
              else return css

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> Bool -> Bool -> Bool -> Package
            -> (Maybe Bool,[Commit]) -- (mancestor,unmerged)
            -> Branch -> Branch -> IO ()
mergeBranch _ _ _ _ _ _ _ _ Rawhide = return ()
mergeBranch _ _ _ _ _ _ (_,[]) _ _ = return ()
mergeBranch _ _ _ _ _ _ (Nothing,_) _ _ = return ()
mergeBranch dryrun nofetch build noprompt showall pkg (Just True, unmerged@(unmgd:_)) from br = do
  when (nofetch && not build) $ putPkgBrnchHdr pkg br
  isnewrepo <- initialPkgRepo
  locals <- localBranches True
  -- FIXME what if branch doesn't exist at all?
  unless (showBranch from `elem` locals) $ do
    git_ "fetch" ["origin"]
    git_ "branch" ["--track", showBranch from, "origin/" ++ showBranch from]
  newerlocal <- gitOneLineLog $ "origin/" ++ showBranch from ++ ".." ++ showBranch from
  unless (null newerlocal) $ do
    putStr "*Warning!* "
    displayHdrCommits ("Unpushed" +-+ showBranch from +-+ "commit") showall newerlocal
    putNewLn
    promptEnter "Press Enter to continue anyway"
  putStrLn $ (if isnewrepo || noprompt then "Merging from" else "New commits in") +-+ "origin/" ++ showBranch from ++ ":"
  displayCommits showall unmerged
  putNewLn
  unpushed <- gitOneLineLog $ "origin/" ++ showBranch br ++ "..HEAD"
  unless (null unpushed) $ do
    displayHdrCommits "Local commit" showall unpushed
    putNewLn
  mmerge <-
    if isnewrepo && length unmerged == 1 || noprompt
    then return $ Just $ commitRef unmgd
    else refPrompt unmerged ("Press Enter to merge from" +-+ showBranch from ++
         (if build then " and build" else "") ++
         (if length unmerged > 1 then "; or give ref to merge" else "") ++
         "; or 'no' to skip merge")
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    unless dryrun $ git_ "merge" ["--quiet", ref]
mergeBranch dryrun nofetch build noprompt showall pkg (Just False,unmerged) from br = do
  unpushed <- gitOneLineLog $ "origin/" ++ showBranch br ++ "..HEAD"
  when (nofetch && not build) $ putPkgBrnchHdr pkg br
  putStrLn $ color (if null unpushed then Magenta else Red) $ showBranch from +-+ "branch is not directly mergeable:"
  displayCommits False unmerged
  putNewLn
  unless (null unpushed) $
    displayHdrCommits "Local commits" showall unpushed
  mmerge <-
    if noprompt && null unpushed
    then return Nothing
    else conflictPrompt unmerged $ "Press Enter to skip merge" ++ (if build then " and build" else "") ++ "; or give ref or 'HEAD' to attempt merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    unless dryrun $
    git_ "merge" [ref]
