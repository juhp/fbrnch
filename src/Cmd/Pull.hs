module Cmd.Pull (
  pullPkgs,
  PullOpt(..)
  )
where

import Branches
import Common
--import Common.System (error')
import Git
import Package

data PullOpt =
  PullLenient | PullNoFetch | PullStash | PullRebase
  deriving Eq

-- FIXME pulling more than one branch
-- FIXME print nvr after pulling or old -> new
pullPkgs :: Maybe PullOpt -> (BranchesReq, [String]) -> IO ()
pullPkgs pullopt (breq,args) =
  withPackagesByBranches
  (if length args > 1 then HeaderMust else HeaderMay)
  False
  (case pullopt of
     Just PullLenient -> Nothing
     Just PullRebase -> Nothing -- FIXME
     Just PullNoFetch -> cleanGit
     Just PullStash -> stashGitFetch
     Nothing -> cleanGitFetch)
  AnyNumber pullPkg (breq,args)
  where
    pullPkg :: Package -> AnyBranch -> IO ()
    pullPkg pkg br =
      if pullopt == Just PullLenient
      then do
        haveGit <- isPkgGitRepo
        if haveGit
          then gitFetchSilent True
          else putStrLn $ "ignoring" +-+ unPackage pkg
      else doPullPkg
      where
        -- FIXME using rebase for branched may be risky
        doPullPkg :: IO ()
        doPullPkg = do
          current <- getReleaseBranchWarn
          unless (breq == Branches [] || RelBranch current == br) $
            gitSwitchBranch br
          if pullopt == Just PullRebase
            then git_ "pull" ["origin"]
            else gitMergeOrigin current
          when (pullopt == Just PullStash) $ do
            stashes <- git "stash" ["list"]
            case line1 stashes of
              (s0,_) | stashedWithFbrnch `isSuffixOf` s0 ->
                       git_ "stash" ["pop", "--quiet"]
              _ -> return ()
