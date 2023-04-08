module Cmd.Diff (
  diffCmd,
  DiffFilter(..),
  DiffFormat(..),
  DiffWork(..)
  ) where

import Branches
import Common
import Common.System
import Git
import Package
import Patch

data DiffFormat =
  DiffDefault | DiffContext Int | DiffMinimal | DiffStatus | DiffStats |
  DiffQuiet
  deriving Eq

data DiffWork =
  DiffWorkAll | DiffWorkUnstage | DiffWorkStaged
  deriving Eq

data DiffFilter =
  DiffMatch String | DiffNotMatch String
  -- | DiffRegex String | DiffNotRegex String
  deriving Eq

-- FIXME diff other branches without switching
-- FIXME --older/--newer branch
diffCmd :: Bool -> Bool -> DiffWork -> DiffFormat -> Bool -> Maybe DiffFilter
        -> Maybe AnyBranch -> (Maybe Branch,[String]) -> IO ()
diffCmd debug speconly work fmt ignorebumps mpatt mwbr =
  withPackagesMaybeBranch (if debug then HeaderMust else HeaderNone) False dirtyGit diffPkg
  where
    diffPkg :: Package -> AnyBranch -> IO ()
    diffPkg pkg br = do
      ok <-
        case br of
          OtherBranch _ -> gitSwitchBranch br >> return True
          RelBranch rbr -> gitSwitchBranch' False rbr
      when ok $ do
        speconlyNone <-
          if speconly
          then notM $ doesFileExist $ packageSpec pkg
          else return False
        if speconlyNone
          then do
          dead <- doesFileExist "dead.package"
          unless dead $ putStrLn $ "no " ++ packageSpec pkg
          else do
          let contxt = case fmt of
                         DiffContext n -> ["--unified=" ++ show n]
                         DiffMinimal -> ["--unified=0"]
                         DiffStatus -> ["--name-status"]
                         -- FIXME hide "files changed" and "insertions" summary
                         DiffStats -> ["--compact-summary"]
                         _ -> []
              (workOpts,workArgs) =
                case work of
                  DiffWorkAll -> ([],["HEAD" | isNothing mwbr])
                  DiffWorkUnstage -> ([],[])
                  DiffWorkStaged -> (["--cached"],[])
              file = [packageSpec pkg | speconly]
          mwithBranch <-
            case mwbr of
              Nothing -> return $ Just []
              Just wbr ->
                let brn = show wbr in
                  if '/' `elem` brn
                  then return $ Just [brn]
                  else do
                    localbrs <- gitLines "branch" ["--format=%(refname:short)"]
                    if brn `elem` localbrs
                      then return $ Just [brn]
                      else do
                      wbexists <- checkIfRemoteBranchExists wbr
                      if wbexists
                        then return $ Just ["origin/" ++ brn]
                        else do
                          putStrLn $ "no" +-+ show wbr +-+ "for" +-+ unPackage pkg
                          return Nothing
          whenJust mwithBranch $ \withBranch -> do
            let revdiff =
                  case mwbr of
                    Nothing -> []
                    Just wbr -> case (wbr,br) of
                      (RelBranch rwbr, RelBranch rbr) -> ["-R" | rwbr > rbr]
                      _ -> []
            diff <- gitLines "diff" $ contxt ++ workOpts ++ revdiff ++ withBranch ++ workArgs ++ file
            let diffout = (maybe id filterPattern mpatt . simplifyDiff fmt) diff
            -- FIXME: sometimes we may want to list even if diff but no diffout
            unless (null diffout) $
              unless (ignorebumps && isTrivialRebuildCommit diffout) $
              if fmt == DiffQuiet
              then putStrLn $ unPackage pkg
              else do
                putPkgAnyBrnchHdr pkg br
                mapM_ putStrLn diffout
        where
          simplifyDiff :: DiffFormat -> [String] -> [String]
          simplifyDiff DiffMinimal ds = simplifyMinimalDiff ds
          -- drop "2 files changed, 113 insertions(+)"
          simplifyDiff DiffStats ds = if null ds then ds else init ds
          simplifyDiff _ ds = ds

          filterPattern :: DiffFilter -> [String] -> [String]
          filterPattern (DiffMatch patt) = filter (patt `isInfixOf`)
          filterPattern (DiffNotMatch patt) = filter (not . (patt `isInfixOf`))
            --  void $ runProcess $ setStderr nullStream $ proc "grep" [pat, file]
