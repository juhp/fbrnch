{-# LANGUAGE OverloadedStrings #-}

module Cmd.Scratch (
  scratchCmd,
  Archs(..),
  ) where

import Branches
import Common
import Common.System
import Git
import Koji
import Package
import Types (Archs(..))

-- FIXME allow multiple --target's (parallel too)
scratchCmd :: Bool -> Bool -> Bool -> Maybe Archs -> Maybe String
           -> Maybe String -> (BranchesReq, [String]) -> IO ()
scratchCmd dryrun rebuildSrpm nofailfast marchopts mtarget mref (breq,pkgs) =
  withPackageByBranches (Just True) Nothing AnyNumber scratchBuild (breq,pkgs)
  where
    scratchBuild :: Package -> AnyBranch -> IO ()
    scratchBuild pkg br = do
      when (isJust mref && length pkgs > 1) $
        error' "--ref is not supported for multiple packages"
      pkggit <- isPkgGitRepo
      when (not pkggit && breq == Branches [] && isNothing mtarget) $
        error' "please specify a branch or target for non dist-git"
      spec <- localBranchSpecFile pkg br
      let target = fromMaybe (anyTarget br) mtarget
      putStrLn $ "Target: " ++ target
      archs <- case marchopts of
        Nothing -> return []
        Just archopts -> case archopts of
          Archs as -> return as
          ExcludedArchs as -> do
            Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
            tagArchs <- kojiTagArchs buildtag
            return $ tagArchs \\ as
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
              if clean then
                null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
                else return False
        rbr <- anyBranchToRelease br
        nvr <- pkgNameVerRel' rbr spec
        putStrLn $ target ++ " scratch build of " ++ fromMaybe nvr mref ++ (if pushed then "" else ".src.rpm")
        unless dryrun $ do
          if pushed
            then kojiBuildBranch target pkg mref $ "--scratch" : kojiargs
            else srpmBuild target kojiargs spec
        else srpmBuild target kojiargs spec
      where
        srpmBuild :: FilePath -> [String] -> String -> IO ()
        srpmBuild target kojiargs spec =
          void $ generateSrpm (Just br) spec >>= kojiScratchBuild target kojiargs

        anyTarget (RelBranch b) = branchTarget b
        anyTarget _ = "rawhide"
