{-# LANGUAGE OverloadedStrings #-}

module Cmd.ListPackages (
  listCmd,
  Packager(..),
  listLocalCmd,
  listPackages
  )
where

import Fedora.Pagure
import SimplePrompt (yesNoDefault)

import Branches
import Common
import Common.System
import Git (gitSwitchBranch')
import Package
import Pagure (srcfpo)

data Packager = Owner String | Committer String

-- FIXME remote/pagure branch and --remote or --no-remote
listCmd :: Bool -> Bool -> Maybe Packager -> [String] -> IO ()
listCmd force count mpackager pkgs = do
  unless (force || count || isJust mpackager || not (null pkgs)) $
    error' "Please give a package pattern, --count, or --owner/--username"
  ecountpkgs <- countListPackages force count mpackager pkgs
  case ecountpkgs of
    Left num -> print num
    Right packages -> mapM_ putStrLn packages

countListPackages :: Bool -> Bool -> Maybe Packager -> [String]
                  -> IO (Either Integer [String])
countListPackages force count mpackager pkgs = do
  emcounts <-
    if null pkgs
    then Left <$> countPackages Nothing
    else Right <$> mapM (countPackages . Just) pkgs
  case emcounts of
      Left mcount ->
        if count
        then return $ Left $ fromMaybe 0 mcount
        else Right <$> doListPackages (mcount, Nothing)
      Right mcounts ->
        if count
        then return $ Left $ sum $ catMaybes mcounts
        else Right <$> concatMapM doListPackages (zip mcounts $ map Just pkgs)
  where
    path = "projects"
    mkParams mpattern =
      makeKey "short" "1" ++ makeKey "fork" "0" ++ packager ++ makeKey "namespace" "rpms" ++ maybeKey "pattern" mpattern
      where
        packager =
          case mpackager of
            Nothing -> makeKey "owner" "!orphan"
            Just (Owner o) -> makeKey "owner" o
            Just (Committer c) -> makeKey "username" c

    countPackages :: Maybe String -> IO (Maybe Integer)
    countPackages mpattern =
      queryPagureCount srcfpo path (mkParams mpattern) "pagination"

    -- FIXME add default --max-pages?
    doListPackages :: (Maybe Integer, Maybe String) -> IO [String]
    doListPackages (Nothing, mpat) =
      error' $ "no matches found" +-+ maybe "" ("for" +-+) mpat
    doListPackages (Just 0, _mpat) = return []
    doListPackages (Just num, mpattern) = do
      ok <-
        if num > 1000 && not force
        then yesNoDefault False $ show num +-+ "results, continue"
        else return True
      if ok
        then do
        pages <- queryPagureCountPaged srcfpo False path (mkParams mpattern) ("pagination", "page")
        return $
          concatMap (map (lookupKey' "name")) $
          mapMaybe (lookupKey "projects") pages
        else error' "aborted"

-- FIXME add --count
listLocalCmd :: (Maybe Branch, [String]) -> IO ()
listLocalCmd =
  withPackagesMaybeBranch HeaderNone False dirtyGit listLocalPkg
  where
    listLocalPkg :: Package -> AnyBranch -> IO ()
    listLocalPkg pkg (RelBranch br) = do
      exists <- gitSwitchBranch' True br
      when exists $
        whenM (isJust <$> maybeFindSpecfile) $
        putStrLn $ unPackage pkg
    listLocalPkg _ (OtherBranch _) =
      error' "other branches not supported yet"

listPackages :: Bool -> Maybe Packager -> [String] -> IO [String]
listPackages force mpackager pkgs =
  either (error' "impossible happened") id <$>
  countListPackages force False mpackager pkgs
