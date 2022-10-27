{-# LANGUAGE OverloadedStrings #-}

module Cmd.Scratch (
  scratchCmd,
  scratchCmdAarch64,
  scratchCmdX86_64,
  Archs(..),
  ) where

import Branches
import Common
import Common.System
import Git
import Koji
import Package
import Types (Archs(..),SideTagTarget)

-- FIXME allow parallel targets
-- FIXME tail build.log for failure
-- FIXME append timestamp after %release (to help identify scratch builds)
scratchCmd :: Bool -> Bool -> Bool -> Bool -> Maybe Archs
           -> [SideTagTarget] -> Maybe String -> (BranchesReq, [String])
           -> IO ()
scratchCmd dryrun stagger rebuildSrpm nofailfast marchopts sidetagTargets mref (breq,pkgs) =
  withPackagesByBranches HeaderMust False Nothing AnyNumber scratchBuild (breq,pkgs)
  where
    anyTarget (RelBranch b) = branchTarget b
    anyTarget _ = "rawhide"

    scratchBuild :: Package -> AnyBranch -> IO ()
    scratchBuild pkg br = do
      when (isJust mref && length pkgs > 1) $
        error' "--ref is not supported for multiple packages"
      pkggit <- isPkgGitRepo
      when (not pkggit && breq == Branches [] && null sidetagTargets) $
        error' "please specify a branch or target for non dist-git"
      spec <- localBranchSpecFile pkg br
      targets <-
        if null sidetagTargets
        then return [anyTarget br]
        else mapM (targetMaybeSidetag dryrun (onlyRelBranch br) . Just) sidetagTargets
      forM_ targets $ \target -> do
        putStrLn $ "Target: " ++ target
        archs <-
          case marchopts of
            Nothing -> return []
            Just archopts ->
              case archopts of
                Archs as -> return as
                ExcludedArchs as -> do
                  Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
                  tagArchs <- kojiTagArchs buildtag
                  return $ tagArchs \\ as
        if stagger
          then do
          archlist <-
            if null archs
            then do
              Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
              tagArchs <- kojiTagArchs buildtag
              -- prioritize preferred archs
              return $ nub $ priorityArchs ++ tagArchs
            else return $ nub $ filter (`elem` archs) priorityArchs ++ archs
          forM_ archlist $ \arch -> do
            putStrLn $ arch ++ " scratch build"
            doScratchBuild pkggit spec target [arch]
          else doScratchBuild pkggit spec target archs
      where
        priorityArchs = ["x86_64", "aarch64", "ppc64le"]

        doScratchBuild pkggit spec target archs = do
          let kojiargs = ["--arch-override=" ++ intercalate "," archs | notNull archs] ++ ["--fail-fast" | not nofailfast && length archs /= 1] ++ ["--no-rebuild-srpm" | not rebuildSrpm]
          if pkggit
            then do
            gitSwitchBranch br
            pushed <- do
              case mref of
                Just ref ->
                  if length ref < 6
                  then error' $ "please use a longer ref: " ++ ref
                  -- FIXME print commit log
                  else return True
                Nothing -> do
                  clean <- isGitDirClean
                  if clean && isRelBranch br
                    then
                    null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
                    else return False
            rbr <- anyBranchToRelease br
            nvr <- pkgNameVerRel' rbr spec
            putStrLn $ target ++ " scratch build of " ++ fromMaybe nvr mref ++ (if pushed then "" else ".src.rpm")
            unless dryrun $ do
              if pushed
                then kojiBuildBranch target pkg mref $ "--scratch" : kojiargs
                else srpmBuild kojiargs
            else srpmBuild kojiargs
          where
            srpmBuild :: [String] -> IO ()
            srpmBuild kojiargs =
              void $ generateSrpm (Just br) spec >>= kojiScratchBuild target kojiargs

-- FIXME default -X to --no-fastfail?
scratchCmdX86_64 :: Bool -> Bool -> Bool -> [SideTagTarget]
                 -> Maybe String -> (BranchesReq, [String]) -> IO ()
scratchCmdX86_64 dryrun rebuildSrpm excludeArch =
  scratchCmd dryrun False rebuildSrpm False (Just (excludeArchs excludeArch ["x86_64"]))

scratchCmdAarch64 :: Bool -> Bool -> Bool -> [SideTagTarget]
                  -> Maybe String -> (BranchesReq, [String]) -> IO ()
scratchCmdAarch64 dryrun rebuildSrpm excludeArch =
  scratchCmd dryrun False rebuildSrpm False (Just (excludeArchs excludeArch ["aarch64"]))

excludeArchs :: Bool -> [String] -> Archs
excludeArchs excl = if excl then ExcludedArchs else Archs
