module Branches (
  packageBranches,
  packageBranched,
  packagePagureBranched,
  switchBranch,
  getCurrentBranch,
  module Distribution.Fedora.Branch
) where

import Common

import Distribution.Fedora.Branch
import SimpleCmd

import Git

getCurrentBranch :: IO (Maybe Branch)
getCurrentBranch = do
  mbr <- cmdMaybe "git" ["rev-parse", "--abbrev-ref", "HEAD"]
  case mbr of
    Nothing -> return Nothing
    Just br -> do
      active <- getFedoraBranches
      return $ readBranch active br

getActiveBranches :: [Branch] -> [String] -> [Branch]
getActiveBranches active =
  -- newest branch first
  {- HLINT ignore "Avoid reverse"-} -- reverse . sort is fast but not stabilizing
  reverse . sort . mapMaybe (readBranch' active)

packageBranches :: IO [Branch]
packageBranches = do
  active <- getFedoraBranches
  getActiveBranches active <$>
    cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-1)"]

packageBranched :: IO [Branch]
packageBranched = do
  active <- getFedoraBranched
  getActiveBranches active <$>
    cmdLines "git" ["branch", "--remote", "--list", "--format=%(refname:lstrip=-1)"]

packagePagureBranched :: String -> IO [Branch]
packagePagureBranched pkg = do
  current <- getFedoraBranched
  getActiveBranches current <$> cmdLines "pagure" ["branches", "rpms/" ++ pkg]

switchBranch :: Branch -> IO ()
switchBranch br = do
  branched <- gitBool "show-ref" ["--verify", "--quiet", "refs/remotes/origin/" ++ show br]
  if not branched
    then error' $ show br ++ " branch does not exist!"
    else do
    current <- git "rev-parse" ["--abbrev-ref", "HEAD"]
    when (current /= show br) $
      cmdSilent "fedpkg" $ "switch-branch" : [show br]
