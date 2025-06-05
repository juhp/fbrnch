module Cmd.SideTags (
  sideTagsCmd,
  SidetagMode(..))
where

import Fedora.Krb (krbTicket)
import SimpleCmd (cmd_, cmdN, error')
import SimplePrompt (yesNo)

import Branches
import Common
import Git (isPkgGitRepo)
import Koji

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
