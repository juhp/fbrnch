{-# LANGUAGE OverloadedStrings #-}

module Cmd.Override (
  overrideCmd,
  OverrideMode(..)
  )
where

import Fedora.Bodhi (bodhiOverride)
import Network.HTTP.Query

import Common
import Common.System

import Bodhi
import Branches
import Cmd.WaitRepo (waitrepoCmd, WaitFetch(WaitNoFetch))
import Git
import Koji
import Package
import Prompt (yesno)

data OverrideMode = OverrideCreate | OverrideList | OverrideExpire
  deriving Eq

-- FIXME debug option
-- FIXME option to expire all overrides
-- FIXME should list all overrides at once
overrideCmd :: Bool -> OverrideMode -> Maybe Int -> Bool
            -> (BranchesReq, [String]) -> IO ()
overrideCmd dryrun mode mduration nowait breqpkgs = do
    unless (nowait || mode /= OverrideCreate) $
      putStrLn "Overriding"
    withPackagesByBranches (if mode == OverrideList then HeaderNone else HeaderMay) False cleanGitFetchActive AnyNumber overrideBranch breqpkgs
    unless (nowait || mode /= OverrideCreate) $
      waitrepoCmd dryrun WaitNoFetch Nothing breqpkgs
  where
    overrideBranch :: Package -> AnyBranch -> IO ()
    overrideBranch _ (OtherBranch _) =
      error' "override only defined for release branches"
    overrideBranch pkg rbr@(RelBranch br) = do
      gitSwitchBranch rbr
      let spec = packageSpec pkg
      checkForSpecFile spec
      nvr <- pkgNameVerRel' br spec
      case mode of
        OverrideCreate -> do
          putStrLn nvr
          tags <- kojiNVRTags nvr
          unless (any (`elem` tags) [show br, show br ++ "-updates", show br ++ "-override"]) $
            unlessM (checkAutoBodhiUpdate br) $
            bodhiCreateOverride dryrun mduration nvr
        OverrideList -> do
          moverride <- bodhiOverride nvr
          whenJust moverride $ \override ->
            case (lookupKey "expired_date" override :: Maybe String) of
              Just date -> putStrLn date
              Nothing ->
                putStrLn $ nvr +-+
                fromMaybe "" (lookupKey "expiration_date" override :: Maybe String)
        OverrideExpire -> do
          moverride <- bodhiOverride nvr
          whenJust moverride $ \override ->
            case (lookupKey "expired_date" override :: Maybe String) of
              Just date -> putStrLn date
              Nothing -> do
                ok <- yesno $ "Expire override" +-+ nvr
                when ok $
                  cmd_ "bodhi" ["overrides", "edit", "--expire", nvr]
