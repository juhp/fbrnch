module Cmd.Bump
  ( bumpCmd,
    bumpPkg
  )
where

import Distribution.Fedora.Branch (branchDestTag)
import System.IO.Extra

import Branches
import Common
import Common.System
import Git
import Koji
import Package

-- FIXME --force
-- FIXME --target
bumpCmd :: Bool -> Bool -> Maybe String -> Maybe String
         -> (BranchesReq,[String]) -> IO ()
bumpCmd dryrun local mcmsg mclog =
  withPackagesByBranches (boolHeader local) False
  (if local then cleanGit else cleanGitFetchActive)
  AnyNumber (bumpPkg dryrun local mcmsg mclog)

bumpPkg :: Bool -> Bool -> Maybe String -> Maybe String
        -> Package -> AnyBranch -> IO ()
bumpPkg dryrun local mcmsg mclog pkg br = do
  dead <- doesFileExist "dead.package"
  if dead
    then putStrLn "dead package"
    else do
    spec <- localBranchSpecFile pkg br
    unpushed <- gitOneLineLog $ "origin/" ++ show br ++ "..HEAD"
    displayCommits True unpushed
    autorelease <- isAutoRelease spec
    if autorelease
      then
      if length unpushed > 0
      then putStrLn $ "autorelease: unpushed" +-+
           case length unpushed of
             1 -> "commit"
             n -> show n +-+ "commits"
      else
        if dryrun
        then putStrLn "autorelease: bump with commit"
        else do
          let copts =
                "-m" :
                case mcmsg of
                  Just msg -> [msg]
                  Nothing ->
                    case mclog of
                      Just cl -> [cl]
                      Nothing -> ["bump release"]
          git_ "commit" $ "-a" : "--allow-empty" : copts
      else do
      rbr <-
        case br of
          RelBranch rbr -> return rbr
          OtherBranch _ -> systemBranch
      newnvr <- pkgNameVerRel' rbr spec
      moldnvr <-
        if local
        then do
          withTempDir $ \tempdir -> do
            git "show" ["origin:" ++ spec] >>=
              writeFile (tempdir </> spec)
            withCurrentDirectory tempdir $ do
              oldautorel <- isAutoRelease (tempdir </> spec)
              if not oldautorel
                then pkgNameVerRel rbr spec
                else do
                -- FIXME check version unchanged before cloning
                clonePkg True AnonClone (Just rbr) $ unPackage pkg
                withCurrentDirectory (unPackage pkg) $
                  pkgNameVerRel rbr spec
        else
          case br of
            RelBranch rbr' ->
              let tag = branchDestTag rbr'
              in kojiLatestNVR tag $ unPackage pkg
            -- FIXME fallback to local?
            _ -> return Nothing
      whenJust moldnvr $ \o -> putStrLn $ showNVR o +-+ "->"
      putStrLn $ showNVR newnvr
      if equivNVR newnvr moldnvr
        then do
        git_ "log" ["origin..HEAD", "--pretty=oneline"]
        let clog =
              case mclog of
                Just cl -> cl
                Nothing ->
                  case mcmsg of
                    Just msg -> msg
                    _ -> "rebuild"
        unless (autorelease || dryrun) $
          cmd_ "rpmdev-bumpspec" ["-c", clog, spec]
        let copts =
              case mcmsg of
                Nothing -> ["-m", "bump release"]
                Just msg -> ["-m", msg]
        -- FIXME quiet commit?
        if dryrun
          then putStrLn "would be bumped with commit"
          else git_ "commit" $ "-a" : (if autorelease then ("--allow-empty" :) else id) copts
        else putStrLn "already bumped"
