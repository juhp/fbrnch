module Cmd.Bump
  ( bumpCmd,
    bumpPkg
  )
where

import System.Console.Pretty (color, Color(Green, Yellow))
import System.IO.Extra

import Branches
import Common
import Common.System
import Git
import Koji
import Package

-- FIXME --force
-- FIXME ignore ":"
-- FIXME countdown
bumpCmd :: Bool -> Bool -> Maybe String -> Maybe String
         -> (BranchesReq,[String]) -> IO ()
bumpCmd dryrun local mcmsg mclog =
  withPackagesByBranches (boolHeader local) False
  (if local then cleanGit else cleanGitFetchActive)
  AnyNumber (bumpPkg dryrun local mcmsg mclog)

bumpPkg :: Bool -> Bool -> Maybe String -> Maybe String
        -> Package -> AnyBranch -> IO ()
bumpPkg dryrun strict mcmsg mclog pkg br = do
  dead <- doesFileExist "dead.package"
  if dead
    then putStrLn "dead package"
    else do
    spec <- localBranchSpecFile pkg br
    unpushed <- gitOneLineLog $ "origin/" ++ show br ++ "..HEAD"
    displayCommits True unpushed
    autorelease <- isAutoRelease spec
    alreadybumped <-
      if autorelease
      then do
        let bumped = not $ null unpushed
        when bumped $
          putStrLn $ "autorelease: unpushed" +-+ pluralOnly unpushed "commit"
        if bumped
          then return True
          else do
          if strict
            then do
            rbr <-
              case br of
                RelBranch rbr -> return rbr
                OtherBranch _ -> error' "unsupported branch"
            newnvr <- pkgNameVerRel' rbr spec
            mstate <- kojiGetBuildState fedoraHub $ BuildInfoNVR $ showNVR newnvr
            let built = mstate == Just BuildComplete
            if built
              then putStrLn $ color Yellow $ showNVR newnvr +-+ "already built"
              else putStrLn $ showNVR newnvr +-+ "not built yet"
            return $ not built
            else return False
      else do
        rbr <-
          case br of
            RelBranch rbr -> return rbr
            OtherBranch _ -> error' "unsupported branch"
        newnvr <- pkgNameVerRel' rbr spec
        moldnvr <-
          withTempDir $ \tempdir -> do
            git "show" ["origin:" ++ spec] >>=
              writeFile (tempdir </> spec)
            withCurrentDirectory tempdir $ do
              oldautorel <- isAutoRelease (tempdir </> spec)
              if not oldautorel
                then pkgNameVerRel rbr spec
                else do
                -- FIXME check version unchanged before cloning
                -- FIXME make silent (no "cloning...")
                clonePkg False True AnonClone (Just rbr) $ unPackage pkg
                withCurrentDirectory (unPackage pkg) $
                  pkgNameVerRel rbr spec
        if equivNVR newnvr moldnvr
          then
          if strict
          then do
            mstate <- kojiGetBuildState fedoraHub $ BuildInfoNVR $ showNVR newnvr
            let built = mstate == Just BuildComplete
            unless built $
              putStrLn $ showNVR newnvr +-+ "not built yet"
            return $ not built
          else return False
          else do
            whenJust moldnvr $ \o -> do
              putStrLn $ showNVR o +-+ "->"
              putStrLn $ showNVR newnvr
            return True
    if alreadybumped
      then putStrLn $ color Green "already bumped"
      else do
      git_ "log" ["origin..HEAD", "--pretty=oneline"]
      let clog =
            case mclog of
              Just cl -> cl
              Nothing ->
                case mcmsg of
                  Just msg -> msg
                  _ -> "Rebuild"
      unless (autorelease || dryrun) $
        cmd_ "rpmdev-bumpspec" ["-c", clog, spec]
      let cmsg = fromMaybe "Bump release" mcmsg
      if dryrun
        then putStrLn $ "would be bumped with commit:" +-+ show cmsg
        -- FIXME quiet commit?
        else git_ "commit" $ "-a" : (if autorelease then ("--allow-empty" :) else id) ["-m", cmsg]
