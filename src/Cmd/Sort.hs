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
import System.Directory (doesFileExist)

import Branches
import Git
import Package
import RpmBuild (distRpmOptions, getDynSourcesMacros, installDeps)

data RpmWith = RpmWith String | RpmWithout String

data SortDisplay = SortParallel | SortChain | SortLayers | SortPlain

-- FIXME ghc-attempt f37 branch does not exist! (coming from f29)
sortCmd :: SortDisplay -> Bool -> Maybe RpmWith -> (Branch,[String]) -> IO ()
sortCmd _ _ _ (_,[]) = return ()
sortCmd displaymode deps mrpmwith (br, pkgs) = do
  withPackagesBranch HeaderNone False Nothing (setupPkg deps) (br, pkgs)
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

setupPkg :: Bool -> Package -> AnyBranch -> IO ()
setupPkg deps pkg br = do
  whenM isPkgGitRepo $ gitSwitchBranch br
  unlessM (doesFileExist "dead.package") $ do
    getDynSourcesMacros $ packageSpec pkg
    when deps $
      localBranchSpecFile pkg br >>= installDeps False False

toRpmOption :: RpmWith -> [String]
toRpmOption (RpmWith opt) = ["--with=" ++ opt]
toRpmOption (RpmWithout opt) = ["--without=" ++ opt]

graphCmd :: Bool -> Bool -> Maybe RpmWith -> (Maybe Branch,[FilePath]) -> IO ()
graphCmd dot deps mrpmwith (mbr, pkgs) = do
  withPackagesMaybeBranchNoHeadergit (setupPkg deps) (mbr, pkgs)
  let rpmopts = maybe [] toRpmOption mrpmwith
  createGraph4 False [] rpmopts False False True Nothing pkgs >>=
    if dot then printGraph else renderGraph
