module Cmd.Sort (
  sortCmd,
  RpmWith(..),
  graphCmd,
  SortDisplay(..)
  )
where

import Control.Monad.Extra
import Data.List (intercalate)
import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order (dependencySortRpmOpts,
                                     dependencySortParallel,
                                     dependencyLayers)

import Branches
import Git
import Package

data RpmWith = RpmWith String | RpmWithout String

data SortDisplay = SortParallel | SortChain | SortLayers | SortPlain

sortCmd :: SortDisplay -> Maybe RpmWith -> (Branch,[String]) -> IO ()
sortCmd _ _ (_,[]) = return ()
sortCmd displaymode mrpmwith (br, pkgs) = do
  withPackagesBranch HeaderNone False Nothing noop (br, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  case displaymode of
    -- reverse because rpmbuild-order reverses the order of independent pkgs?
    SortParallel ->
      dependencySortParallel (reverse pkgs) >>= mapM_ (putStrLn . unwords)
    SortChain ->
      dependencyLayers pkgs >>=
      putStrLn . intercalate " : " . map unwords
    SortLayers ->
      dependencyLayers pkgs >>=
      mapM_ (putStrLn . unwords)
    SortPlain ->
      dependencySortRpmOpts rpmopts (reverse pkgs) >>= putStrLn . unwords

noop :: Package -> AnyBranch -> IO ()
noop _pkg br =
  whenM isPkgGitRepo $ gitSwitchBranch br

toRpmOption :: RpmWith -> [String]
toRpmOption (RpmWith opt) = ["--with=" ++ opt]
toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

graphCmd :: Bool -> Maybe RpmWith -> (Maybe Branch,[FilePath]) -> IO ()
graphCmd dot mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranchNoHeadergit noop (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  createGraph4 False [] rpmopts False False True Nothing pkgs >>=
    if dot then printGraph else renderGraph
