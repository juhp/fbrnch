module Cmd.Sort (
  sortCmd,
  RpmWith(..),
  graphCmd
  )
where

import Control.Monad.Extra
import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order (dependencySortRpmOpts,
                                     dependencySortParallel)

import Branches
import Git
import Package

data RpmWith = RpmWith String | RpmWithout String

sortCmd :: Bool -> Maybe RpmWith -> (Maybe Branch,[String]) -> IO ()
sortCmd _ _ (_,[]) = return ()
sortCmd parallel mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranchNoHeadergit ExactlyOne noop (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  if parallel
    -- reverse because rpmbuild-order reverses the order of independent pkgs?
    then dependencySortParallel (reverse pkgs) >>= mapM_ (putStrLn . unwords)
    else dependencySortRpmOpts rpmopts (reverse pkgs) >>= putStrLn . unwords

noop :: Package -> AnyBranch -> IO ()
noop _pkg br =
  whenM isPkgGitRepo $ gitSwitchBranch br

toRpmOption :: RpmWith -> [String]
toRpmOption (RpmWith opt) = ["--with=" ++ opt]
toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

graphCmd :: Bool -> Maybe RpmWith -> (Maybe Branch,[FilePath]) -> IO ()
graphCmd dot mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranchNoHeadergit ZeroOrOne noop (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  createGraph'''' False [] rpmopts False False True Nothing pkgs >>=
    if dot then printGraph else renderGraph
