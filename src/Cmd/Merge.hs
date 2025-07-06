module Cmd.Merge (
  mergeCmd,
  mergeBranch,
  getNewerBranch)
where

import System.Console.Pretty (color, Color(Magenta, Red))
import Safe (tailSafe)

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
      exists <- gitSwitchBranch' False br
      when exists $ do
        mfrom' <- if isJust mfrom
                  then return mfrom
                  else getNewerBranch (unPackage pkg) br
        whenJust mfrom' $ \from -> do
          when (from == br) $
            error' "cannot merge branch to itself"
          unless dryrun $
            gitMergeOrigin br
          (ancestor,unmerged) <- mergeable from br
          unmerged' <- filterOutTrivial mnotrivial unmerged
          mergeBranch dryrun nofetch False noprompt showall pkg (ancestor,unmerged') from br
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

-- FIXME maybe require local branch already here
mergeable :: Branch -> Branch -> IO (Bool,[Commit])
mergeable _ Rawhide = return (False,[])
mergeable from to = do
  (mancestor, unmerged) <- gitMergeable to from
  return (mancestor == Just True, unmerged)

-- FIXME return merged ref
mergeBranch :: Bool -> Bool -> Bool -> Bool -> Bool -> Package
            -> (Bool,[Commit]) -- (ancestor,unmerged)
            -> Branch -> Branch -> IO ()
mergeBranch _ _ _ _ _ _ _ _ Rawhide = return ()
mergeBranch _ _ _ _ _ _ (_,[]) _ _ = return ()
mergeBranch dryrun nofetch build noprompt showall pkg (True, unmerged@(unmgd:_)) from br = do
  when (nofetch && not build) $ putPkgBrnchHdr pkg br
  isnewrepo <- initialPkgRepo
  putStrLn $ (if isnewrepo || noprompt then "Merging from" else "New commits in") +-+ "origin/" ++ showBranch from ++ ":"
  displayCommits showall unmerged
  putNewLn
  unpushed <- gitOneLineLog $ "origin/" ++ showBranch br ++ "..HEAD"
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    displayCommits showall unpushed
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
  whenJust mmerge $ \ ref -> do
    locals <- localBranches True
    unless (showBranch from `elem` locals) $
      git_ "fetch" ["origin", showBranch from ++ ":" ++ showBranch from]
    unless dryrun $
      -- FIXME merge from origin by default not local branch
      git_ "merge" ["--quiet", ref]
mergeBranch dryrun nofetch build noprompt showall pkg (False,unmerged) from br = do
  unpushed <- gitOneLineLog $ "origin/" ++ showBranch br ++ "..HEAD"
  when (nofetch && not build) $ putPkgBrnchHdr pkg br
  putStrLn $ color (if null unpushed then Magenta else Red) $ showBranch from +-+ "branch is not directly mergeable:"
  displayCommits False unmerged
  putNewLn
  unless (null unpushed) $ do
    putStrLn "Local commits:"
    displayCommits showall unpushed
  mmerge <-
    if noprompt && null unpushed
    then return Nothing
    else conflictPrompt unmerged $ "Press Enter to skip merge" ++ (if build then " and build" else "") ++ "; or give ref or 'HEAD' to attempt merge"
  -- ensure still on same branch!
  gitSwitchBranch (RelBranch br)
  whenJust mmerge $ \ ref ->
    unless dryrun $
    git_ "merge" [ref]
