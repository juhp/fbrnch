{-# LANGUAGE OverloadedStrings #-}

module Cmd.Override (
  overrideCmd,
  OverrideMode(..)
  )
where

import Data.Aeson (Object)
import Fedora.Bodhi (bodhiOverrides)
import Network.HTTP.Query
import SimplePrompt (yesNo)

import Common
import Common.System

import Bodhi
import Branches
import Cmd.WaitRepo (waitrepoCmd, WaitFetch(WaitNoFetch))
import Git
import Koji
import Krb (krbTicket)
import Package

data OverrideMode = OverrideCreate | OverrideList | OverrideExpire
  deriving Eq

-- FIXME debug option?
overrideCmd :: Bool -> OverrideMode -> Maybe Int -> Bool
            -> (BranchesReq, [String]) -> IO ()
overrideCmd dryrun OverrideCreate mduration nowait breqpkgs = do
  krbTicket
  unless nowait $
    putStrLn "Overriding"
  withPackagesByBranches HeaderMay False cleanGitFetchActive AnyNumber overrideBranch breqpkgs
  unless nowait $
    waitrepoCmd dryrun True WaitNoFetch Nothing breqpkgs
  where
    overrideBranch :: Package -> AnyBranch -> IO ()
    overrideBranch _ (OtherBranch _) =
      error' "override only defined for release branches"
    overrideBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      checkForSpecFile spec
      nvr <- pkgNameVerRel' br spec
      putStrLn $ showNVR nvr
      tags <- kojiNVRTags nvr
      unless (any (`elem` tags) [showBranch br, showBranch br ++ "-updates", showBranch br ++ "-override"]) $
        unlessM (checkAutoBodhiUpdate br) $
        bodhiCreateOverride dryrun mduration nvr
overrideCmd _dryrun OverrideList _mduration _nowait (_breq,pkgs) =
  withPackages pkgs $
  packageOverrides >=> mapM_ showOverride
  where
    showOverride override =
      case (lookupKey "expired_date" override :: Maybe String) of
        Just _expiry -> return ()
        Nothing ->
          whenJust (lookupKey "nvr" override) $ \nvr ->
          putStrLn $ nvr +-+
          fromMaybe "" (lookupKey "expiration_date" override :: Maybe String)
overrideCmd _dryrun OverrideExpire _mduration _nowait (_breq,pkgs) =
  withPackages pkgs $
  packageOverrides >=> mapM_ expireOverride
  where
    expireOverride override =
      case (lookupKey "expired_date" override :: Maybe String) of
        Just _expired -> return ()
        Nothing -> do
          whenJust (lookupKey "nvr" override) $ \nvr -> do
            ok <- yesNo $ "Expire override" +-+ nvr
            when ok $
              cmd_ "bodhi" ["overrides", "edit", "--expire", nvr]

withPackages :: [FilePath] -> (Package -> IO ()) -> IO ()
withPackages pkgs act =
  forM_ (if null pkgs then ["."] else pkgs) $ \pkgdir ->
  withExistingDirectory pkgdir $
  getPackageName "." >>= act

packageOverrides :: Package -> IO [Object]
packageOverrides pkg =
  -- FIXME could filter by "releases" for Branch's
  bodhiOverrides [makeItem "packages" (unPackage pkg),
                  makeItem "expired" "0"]
