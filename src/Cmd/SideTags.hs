module Cmd.SideTags (
  sideTagsCmd,
  SidetagMode(..),
  tagBuildToSidetagCmd)
where

import Fedora.Krb (krbTicket)
import SimpleCmd (cmd_, cmdN, error')
import SimplePrompt (yesNo)

import Branches
import Common
import Git (gitSwitchBranchVerbose, isPkgGitRepo)
import Koji
import Package (cleanGit, dirtyGit, findSpecfile, pkgNameVerRel',
                withPackagesMaybeBranch, HeaderShow(HeaderMust))

data SidetagMode = SidetagAdd | SidetagRemove | SidetagTagged
  deriving Eq

sideTagsCmd :: Bool -> Maybe SidetagMode -> [Branch] -> IO ()
sideTagsCmd dryrun mmode brs = do
  branches <-
    if null brs && isJust mmode
    then do
      pkggit <- isPkgGitRepo
      if pkggit
        then do
        br <- getReleaseBranch
        return [br]
        else error' "please specify a branch"
    else return brs
  sidetags <-
    sort <$>
    if null branches
    then kojiUserSideTags Nothing
    else concatMapM (kojiUserSideTags . Just) branches
  unless (isNothing mmode)
    krbTicket
  case mmode of
    Nothing -> mapM_ putStrLn sidetags
    Just SidetagTagged -> mapM_ taggedSideTag sidetags
    Just SidetagRemove -> mapM_ removeSideTag sidetags
    Just SidetagAdd -> do
      putStrLn "existing tags:"
      mapM_ putStrLn sidetags
      putNewLn
      mapM_ addSideTag branches
  where
    removeSideTag :: String -> IO ()
    removeSideTag tag =
      whenM (yesNo $ "Remove" +-+ tag) $
      (if dryrun then cmdN else cmd_) "fedpkg" ["remove-side-tag", tag]

    addSideTag :: Branch -> IO ()
    addSideTag br =
      whenM (yesNo $ "Create" +-+ indefinite (showBranch br) +-+ "user sidetag") $
      void $ createKojiSidetag dryrun br

    -- FIXME can we get koji-hs to do this?
    taggedSideTag :: String -> IO ()
    taggedSideTag tag = do
      putStrLn $ "#" +-+ tag
      cmd_ "koji" ["list-tagged", tag]

tagBuildToSidetagCmd :: Bool -> Bool -> Branch -> (Maybe Branch,[FilePath]) -> IO ()
tagBuildToSidetagCmd dryrun allowdirty sidebr (mbr, pkgs) = do
  when (isNothing mbr && length pkgs > 1) $
    error' "Please specify source branch for multiple packages"
  sidetags <- fmap sort <$> kojiUserSideTags $ Just sidebr
  case sidetags of
    [] -> error' $ "No sidetag for" +-+ showBranch sidebr
    [sidetag] -> do
      putStrLn $ "using" +-+ sidetag ++ "\n"
      withPackagesMaybeBranch HeaderMust False (if allowdirty then dirtyGit else cleanGit) (sideTagBuild sidetag) (mbr, pkgs)
    _ -> error' $ "More than one sidetag found:\n" +-+ unwords sidetags
  where
    sideTagBuild sidetag _pkg (RelBranch br) = do
      pkggit <- isPkgGitRepo
      if pkggit
        then do
        gitSwitchBranchVerbose True False $ RelBranch br
        spec <- findSpecfile
        nvr <- pkgNameVerRel' br spec
        ok <- yesNo $ "Tag" +-+ show (showNVR nvr) +-+ "into" +-+ show sidetag
        when ok $
          if dryrun
          then putStrLn "tag-build skipped for dry-run"
          -- FIXME check pkg listed in koji
          else cmd_ "koji" ["tag-build", sidetag, showNVR nvr]
        else error' "needs to be run in dist-git dir and with correct branch"
    sideTagBuild _ _ _ = error' "only for release branches"
