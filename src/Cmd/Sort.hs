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
import RpmBuild (distRpmOptions, getDynSourcesMacros)

data RpmWith = RpmWith String | RpmWithout String

data SortDisplay = SortParallel | SortChain | SortLayers | SortPlain

-- FIXME ghc-attempt f37 branch does not exist! (coming from f29)
sortCmd :: SortDisplay -> Maybe RpmWith -> (Branch,[String]) -> IO ()
sortCmd _ _ (_,[]) = return ()
sortCmd displaymode mrpmwith (br, pkgs) = do
  withPackagesBranch HeaderNone False Nothing setupPkg (br, pkgs)
  distopts <- distRpmOptions br
  let rpmopts = maybe [] toRpmOption mrpmwith ++ distopts
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

setupPkg :: Package -> AnyBranch -> IO ()
setupPkg pkg br = do
  whenM isPkgGitRepo $ gitSwitchBranch br
  getDynSourcesMacros $ packageSpec pkg

toRpmOption :: RpmWith -> [String]
toRpmOption (RpmWith opt) = ["--with=" ++ opt]
toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

graphCmd :: Bool -> Maybe RpmWith -> (Maybe Branch,[FilePath]) -> IO ()
graphCmd dot mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranchNoHeadergit setupPkg (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  createGraph4 False [] rpmopts False False True Nothing pkgs >>=
    if dot then printGraph else renderGraph
