module Cmd.Sort (
  sortCmd,
  RpmWith(..),
  graphCmd
  )
where

import Control.Monad.Extra
import Distribution.RPM.Build.Graph -- (dependencySortRpmOpts)
import Distribution.RPM.Build.Order (dependencySortRpmOpts)

import Branches
import Git
import Package

data RpmWith = RpmWith String | RpmWithout String

sortCmd :: Maybe RpmWith -> (Maybe Branch,[String]) -> IO ()
sortCmd _ (_,[]) = return ()
sortCmd mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranch Nothing Nothing ExactlyOne noop (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  packages <- dependencySortRpmOpts rpmopts $ reverse pkgs
  putStrLn $ unwords packages

noop :: Package -> AnyBranch -> IO ()
noop _pkg br =
  whenM isPkgGitRepo $ gitSwitchBranch br

toRpmOption :: RpmWith -> [String]
toRpmOption (RpmWith opt) = ["--with=" ++ opt]
toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

graphCmd :: Bool -> Maybe RpmWith -> (Maybe Branch,[FilePath]) -> IO ()
graphCmd dot mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranch Nothing Nothing ZeroOrOne noop (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  createGraph'''' False [] rpmopts False False True Nothing pkgs >>=
    if dot then printGraph else renderGraph
