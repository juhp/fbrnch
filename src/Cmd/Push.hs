module Cmd.Push (
  pushPkgs
  )
where

import Control.Monad.Extra (when, whenJustM)
import Data.Maybe (isJust)
import SimpleCmd (error', (+-+))

import Branches
import Git
import Package

pushPkgs :: Bool -> Bool -> Maybe String -> (BranchesReq, [String]) -> IO ()
pushPkgs dryrun nofetch mref (breq, pkgs) = do
  when (isJust mref && length pkgs > 1) $
    error' "can only specify ref for single package"
  withPackagesByBranches HeaderMust False (if nofetch then cleanGit else cleanGitFetch) AnyNumber pushPkg (breq, pkgs)
  where
    pushPkg :: Package -> AnyBranch -> IO ()
    pushPkg _pkg (RelBranch br) = do
      exists <- gitSwitchBranch' True br
      if exists
        then do
        whenJustM (gitShortLog1 $ Just $ "origin/" ++ showBranch br ++ "..HEAD") $ putStrLn . showCommit
        if dryrun
          then checkOnBranch
          else gitPush False mref
        else error' $ "no branch:" +-+ showBranch br
    pushPkg _ (OtherBranch _) = error' "pushing to non-release branches unsupported"
