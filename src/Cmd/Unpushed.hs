{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.Unpushed (
  unpushedCmd
  )
where


import Branches
import Cmd.Bump (bumpPkg)
import Common
import Common.System
import Git
import Package

unpushedCmd :: Bool -> Bool -> Bool -> (BranchesReq,[String]) -> IO ()
unpushedCmd checknvr latest bump (breq, pkgs) =
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
        putStrLn $ name +-+ "has no branch" +-+ showBranch br
        else do
        gitSwitchBranch rbr
        let spec = packageSpec pkg
            prefix =
              let pref =
                    (if length pkgs > 1 && latest
                     then unPackage pkg else "") +-+
                    case breq of
                      Branches brs | length brs <= 1 -> ""
                      _ -> showBranch br
              in if null pref then "" else pref ++ ":"
        haveSpec <- doesFileExist spec
        if not haveSpec
          then
          ifM initialPkgRepo
          (putStrLn $ prefix +-+ "initial repo") $
          unlessM (doesFileExist "dead.package") $
          putStrLn $ prefix +-+ "missing" +-+ spec
          else do
          when (checknvr || length pkgs < 10) $
            whenM (isNothing <$> pkgNameVerRel br spec) $ do
            putStrLn "undefined NVR!\n"
            putStr "HEAD "
          unpushed <- gitShortLogN (if latest then Just 1 else Nothing) $
                      Just $ "origin/" ++ showBranch br ++ "..HEAD"
          if null unpushed
            then
            when bump $ bumpPkg False False Nothing Nothing pkg rbr
            else
            if latest
            then whenJust (listToMaybe unpushed) $ putCommit prefix
            else mapM_ (putCommit prefix) unpushed

    putCommit prefix = putStrLn . (prefix +-+) . showCommit
