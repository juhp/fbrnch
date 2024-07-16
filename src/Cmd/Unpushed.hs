{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Unpushed (
  unpushedCmd
  )
where

import Common
import Common.System

import Branches
import Git
import Package

unpushedCmd :: Bool -> Bool -> (BranchesReq,[String]) -> IO ()
unpushedCmd latest bump (breq, pkgs) =
  -- FIXME dirty not okay for multiple branches?
  withPackagesByBranches (if latest then HeaderMay else HeaderMust) False dirtyGit AnyNumber unpushedBranch (breq, pkgs)
  where
    -- FIXME note dirty when local changes
    unpushedBranch :: Package -> AnyBranch -> IO ()
    unpushedBranch _ (OtherBranch _) =
      error' "status currently only defined for release branches"
    unpushedBranch pkg rbr@(RelBranch br) = do
      brExists <- checkIfRemoteBranchExists rbr
      if not brExists
        then do
        name <- getDirectoryName
        putStrLn $ name +-+ "has no branch" +-+ show br
        else do
        gitSwitchBranch rbr
        let spec = packageSpec pkg
            prefix =
              let pref =
                    (if length pkgs > 1 && latest
                     then unPackage pkg else "") +-+
                    case breq of
                      Branches brs | length brs <= 1 -> ""
                      _ -> show br
              in if null pref then "" else pref ++ ":"
        haveSpec <- doesFileExist spec
        if not haveSpec
          then
          ifM initialPkgRepo
          (putStrLn $ prefix +-+ "initial repo") $
          ifM (doesFileExist "dead.package")
          (putStrLn $ prefix +-+ "dead package") $
          putStrLn $ prefix +-+ "missing" +-+ spec
          else do
          whenM (isNothing <$> pkgNameVerRel br spec) $ do
            putStrLn "undefined NVR!\n"
            putStr "HEAD "
          unpushed <- gitShortLogN (if latest then Just 1 else Nothing) $
                      Just $ "origin/" ++ show br ++ "..HEAD"
          if null unpushed
            then
            when bump $ doBump spec
            else
            if latest
            then whenJust (listToMaybe unpushed) $ putCommit prefix
            else mapM_ (putCommit prefix) unpushed

    putCommit prefix = putStrLn . (prefix +-+) . showCommit

    doBump spec = do
      checkWorkingDirClean False
      dead <- doesFileExist "dead.package"
      if dead
        then putStrLn "dead package"
        else do
        putStrLn "bumping"
        autorelease <- isAutoRelease spec
        unless autorelease $
          cmd_ "rpmdev-bumpspec" ["-c", "rebuild", spec]
        git_ "commit" $ "-a" : (if autorelease then ("--allow-empty" :) else id) ["-m", "bump release"]
