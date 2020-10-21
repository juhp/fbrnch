{-# LANGUAGE OverloadedStrings #-}

module Cmd.Scratch (
  scratchCmd,
  Archs(..),
  ) where

import Branches
import Common
import Git
import Koji
import Package

data Archs = Archs [String] | ExcludedArchs [String]

-- FIXME default to rawhide/master?
-- FIXME build from a specific git ref
-- FIXME print message about uploading srpm
scratchCmd :: Bool -> Bool -> Bool -> Maybe Archs -> Maybe String -> [String]
           -> IO ()
scratchCmd dryrun rebuildSrpm nofailfast marchopts mtarget =
  withPackageByBranches (Just False) Nothing Nothing AnyNumber scratchBuild
  where
    scratchBuild :: Package -> AnyBranch -> IO ()
    scratchBuild pkg br = do
      spec <- localBranchSpecFile pkg br
      let target = fromMaybe (anyTarget br) mtarget
      archs <- case marchopts of
        Nothing -> return []
        Just archopts -> case archopts of
          Archs as -> return as
          ExcludedArchs as -> do
            Just (buildtag,_desttag) <- kojiBuildTarget fedoraHub target
            tagArchs <- kojiTagArchs buildtag
            return $ tagArchs \\ as
      let kojiargs = ["--arch-override=" ++ intercalate "," archs | notNull archs] ++ ["--fail-fast" | not nofailfast] ++ ["--no-rebuild-srpm" | not rebuildSrpm]
      pkggit <- isPkgGitRepo
      if pkggit
        then do
        gitSwitchBranch br
        pushed <- do
          clean <- isGitDirClean
          if clean then
            null <$> gitShortLog ("origin/" ++ show br ++ "..HEAD")
            else return False
        unless dryrun $ do
          if pushed then do
            void $ getSources spec
            kojiBuildBranch target pkg Nothing $ "--scratch" : kojiargs
            else srpmBuild target kojiargs spec
          else srpmBuild target kojiargs spec
      where
        srpmBuild :: FilePath -> [String] -> String -> IO ()
        srpmBuild target kojiargs spec =
          void $ generateSrpm (Just br) spec >>= kojiScratchBuild target kojiargs

        anyTarget (RelBranch b) = branchTarget b
        anyTarget _ = "rawhide"
