{-# LANGUAGE OverloadedStrings #-}

module Cmd.Scratch (
  scratchCmd,
  scratchCmdAarch64,
  scratchCmdX86_64,
  Archs(..),
  ScratchSource(..)
  ) where

import Data.RPM.NVR (NVR)
import Safe (tailSafe)

import Branches
import Common
import Common.System
import Git
import Koji
import Package
import RpmBuild (generateSrpm)
import Types (Archs(..),SideTagTarget)

data ScratchSource = ScratchRef String | ScratchSRPM String

showScratchSource :: Bool -> NVR -> Maybe ScratchSource -> String
showScratchSource pushed nvr Nothing =
  showNVR nvr  ++ (if pushed then "" else ".src.rpm")
showScratchSource _ _ (Just (ScratchRef ref)) = ref
showScratchSource _ _ (Just (ScratchSRPM srpm)) = srpm

-- FIXME --no-tail
-- FIXME --with --without ?
-- FIXME allow parallel targets
-- FIXME append timestamp after %release (to help identify scratch builds)
-- FIXME FIXME use option type:
scratchCmd :: Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Archs
           -> [SideTagTarget] -> Maybe ScratchSource -> (BranchesReq, [String])
           -> IO ()
scratchCmd dryrun stagger rebuildSrpm nofailfast allowHEAD marchopts sidetagTargets msource (breq,pkgs) =
  withPackagesByBranches HeaderMust False Nothing AnyNumber scratchBuild (breq,pkgs)
  where
    anyTarget (RelBranch b) = showBranch b
    anyTarget _ = "rawhide"

    scratchBuild :: Package -> AnyBranch -> IO ()
    scratchBuild pkg br = do
      when (isJust msource && length pkgs > 1) $
        error' "source override is not supported for multiple packages"
      pkggit <- isPkgGitRepo
      when (not pkggit && breq == Branches [] && null sidetagTargets) $
        error' "please specify a branch or target for non dist-git"
      spec <- localBranchSpecFile pkg br
      targets <-
        if null sidetagTargets
        then return [anyTarget br]
        else mapM (targetMaybeSidetag dryrun False False (onlyRelBranch br) . Just) sidetagTargets
      hub <- getKojiProfileHub
      forM_ targets $ \target -> do
        archs <-
          case marchopts of
            Nothing -> return []
            Just archopts ->
              case archopts of
                Archs as -> return as
                ExcludedArchs as -> do
                  (buildtag,_desttag) <- kojiBuildTarget' hub target
                  tagArchs <- kojiTagArchs buildtag
                  excludedarchs <- do
                    excluded <- map words . filter ("ExcludeArch:" `isPrefixOf`) <$> cmdLines "rpmspec" ["-P", spec]
                    return $
                      if null excluded
                      then return []
                      else concatMap tailSafe excluded
                  return $ tagArchs \\ (map normalArch as ++ excludedarchs)
        if stagger
          then do
          archlist <-
            if null archs
            then do
              (buildtag,_desttag) <- kojiBuildTarget' hub target
              tagArchs <- kojiTagArchs buildtag
              -- prioritize preferred archs
              return $ nub $ priorityArchs ++ tagArchs
            else return $ nub $ filter (`elem` archs) priorityArchs ++ archs
          forM_ archlist $ \arch -> do
            putStrLn $ arch +-+ "scratch build"
            doScratchBuild pkggit spec target [arch]
          else doScratchBuild pkggit spec target archs
      where
        priorityArchs = ["x86_64", "aarch64", "ppc64le"]

        normalArch "i386" = "i686"
        normalArch s = s

        doScratchBuild pkggit spec target archs = do
          let kojiargs = ["--arch-override=" ++ intercalate "," archs | notNull archs] ++ ["--fail-fast" | not nofailfast && length archs /= 1] ++ ["--no-rebuild-srpm" | not rebuildSrpm]
          if pkggit
            then do
            gitSwitchBranchVerbose False allowHEAD br
            pushed <- do
              case msource of
                Just (ScratchRef ref) ->
                  if length ref < 6
                  then error' $ "please use a longer ref:" +-+ ref
                  -- FIXME print commit log
                  else return True
                Just (ScratchSRPM _) -> return False
                Nothing -> do
                  clean <- isGitDirClean
                  if clean && isRelBranch br
                    then
                    null <$> gitOneLineLog ("origin/" ++ show br ++ "..HEAD")
                    else return False
            rbr <- anyBranchToRelease br
            nvr <- pkgNameVerRel' rbr spec
            putStrLn $ target +-+ "scratch build of" +-+ showScratchSource pushed nvr msource
            unless dryrun $
              case msource of
                Just (ScratchSRPM srpm) ->
                  void $ kojiScratchBuild target kojiargs srpm
                Nothing ->
                  if pushed
                  then kojiBuildBranch target pkg Nothing $ "--scratch" : kojiargs
                  else srpmBuild kojiargs
                Just (ScratchRef ref) ->
                  kojiBuildBranch target pkg (Just ref) $ "--scratch" : kojiargs
            else srpmBuild kojiargs
          where
            srpmBuild :: [String] -> IO ()
            srpmBuild kojiargs = do
              putStrLn $ "Target:" +-+ target
              void $ generateSrpm (Just br) spec >>= kojiScratchBuild target kojiargs

scratchCmdX86_64 :: Bool -> Bool -> Bool -> Bool -> [SideTagTarget]
                 -> Maybe ScratchSource -> (BranchesReq, [String]) -> IO ()
scratchCmdX86_64 dryrun rebuildSrpm allowHEAD excludeArch =
  scratchCmd dryrun False rebuildSrpm True allowHEAD (Just (excludeArchs excludeArch ["x86_64"]))

scratchCmdAarch64 :: Bool -> Bool -> Bool -> Bool -> [SideTagTarget]
                  -> Maybe ScratchSource -> (BranchesReq, [String]) -> IO ()
scratchCmdAarch64 dryrun rebuildSrpm allowHEAD excludeArch =
  scratchCmd dryrun False rebuildSrpm True allowHEAD (Just (excludeArchs excludeArch ["aarch64"]))

excludeArchs :: Bool -> [String] -> Archs
excludeArchs excl = if excl then ExcludedArchs else Archs
