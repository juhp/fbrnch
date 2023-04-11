{-# LANGUAGE CPP, OverloadedStrings #-}

module Cmd.RequestBranch (
  requestBranchesCmd,
  requestPkgBranches
  ) where

import Network.HTTP.Query (lookupKey')

import Common
import Common.System

import Branches
import Bugzilla
import Cmd.SrcDeps (srcDeps)
import Git
import Krb
import ListReviews
import Package
import Pagure
import SimplePrompt (prompt_)

-- FIXME option to do koji scratch build instead of mock
requestBranchesCmd :: Bool -> Maybe Branch -> Bool -> (BranchesReq,[String]) -> IO ()
requestBranchesCmd quiet mrecursebr mock (breq, ps) = do
  if null ps
    then do
    when (isJust mrecursebr) $
      error' "please specify a package dir when using --recurse-deps"
    isPkgGit <- isPkgGitSshRepo
    if isPkgGit
      then
      getDirectoryName >>= requestPkgBranches quiet False mock breq . Package
      else do
      pkgs <- map reviewBugToPackage <$> listReviews ReviewUnbranched
      mapM_ (\ p -> withExistingDirectory p $ requestPkgBranches quiet (length pkgs > 1) mock breq (Package p)) pkgs
    else do
    pkgs <-
      case mrecursebr of
        Just br -> do
          deps <- concat <$> srcDeps False (br,ps)
          putStrLn $ unwords deps
          unless quiet $
            prompt_ "\nPress Enter to check these packages for branches"
          return deps
        Nothing -> return ps
    forM_ pkgs $ \ p ->
      withExistingDirectory p $ do
      pkg <- getDirectoryName
      requestPkgBranches quiet (length pkgs > 1) mock breq (Package pkg)

requestPkgBranches :: Bool -> Bool -> Bool -> BranchesReq -> Package -> IO ()
requestPkgBranches quiet multiple mock breq pkg = do
  when (breq == Branches []) $
    putPkgHdr pkg
  brs <- localBranches False
  branches <- getRequestedBranches brs breq
  if null branches
    then
    unless quiet $ do
    when multiple $ putStr $ unPackage pkg ++ " "
    case breq of
      Branches [_] -> putStrLn "exists"
      _ -> putStrLn "branches exist"
    else do
    when multiple $ putStr $ unPackage pkg ++ " "
    gitFetchSilent True
    brs' <- localBranches False
    branches' <- getRequestedBranches brs' (Branches branches)
    whenM (havePkgAccess pkg) $ do
      newbranches <- filterExistingBranchRequests branches'
      unless (null newbranches) $ do
        mbidsession <- bzReviewSession
        urls <- forM newbranches $ \ br -> do
          when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br Nothing]
          when (length branches' > 1) $ putStr $ show br ++ " "
          -- 1. Can timeout like this:
          -- Could not execute request_branch: HTTPSConnectionPool(host='pagure.io', port=443): Read timed out. (read timeout=60)
          -- fbrnch: readCreateProcess: fedpkg "request-branch" "epel9" (exit 1): failed
          -- &
          -- 2. Can fail like this:
          -- Could not execute request_branch: The following error occurred while trying to get the active release branches in PDC: <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
          -- <title>500 Internal Server Error</title>
          -- <p>The server encountered an internal error or
          -- misconfiguration and was unable to complete
          -- your request.</p> [...]
          -- fbrnch: readCreateProcess: fedpkg "request-branch" "epel9" (exit 1): failed
          u <- fedpkg "request-branch" [show br]
          putStrLn u
          return u
        whenJust mbidsession $ \(bid,session) ->
          commentBug session bid $ unlines urls
  where
    -- doRequestBr :: Bool -> Branch -> IO String
    -- doRequestBr multibr br = do
    --   when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br]
    --   when multiple $ putStr (unPackage pkg ++ " ")
    --   when multibr $ putStr (show br)
    --   -- Can timeout like this:
    --   -- Could not execute request_branch: HTTPSConnectionPool(host='pagure.io', port=443): Read timed out. (read timeout=60)
    --   -- fbrnch: readCreateProcess: fedpkg "request-branch" "epel9" (exit 1): failed
    --   u <- fedpkg "request-branch" [show br]
    --   putStrLn $ ' ' : u
    --   return $ show br +-+ u

    filterExistingBranchRequests :: [Branch] -> IO [Branch]
    filterExistingBranchRequests branches = do
      existing <- fedoraBranchesNoRawhide (localBranches True)
      let pkgPrefix = if multiple then unPackage pkg ++ ": " else ""
      forM_ branches $ \ br ->
        when (br `elem` existing) $
        putStrLn $ pkgPrefix ++ show br +-+ "branch already exists"
      let brs' = branches \\ existing
      if null brs'
        then return []
        else do
        current <- fedoraBranchesNoRawhide $ pagurePkgBranches (unPackage pkg)
        forM_ brs' $ \ br ->
          when (br `elem` current) $
          putStrLn $ pkgPrefix ++ show br +-+ "remote branch already exists"
        let newbranches = brs' \\ current
        if null newbranches then return []
          else do
          fasid <- fasIdFromKrb
          -- FIXME retry on HttpExceptionRequest ... ConnectionTimeout
          erecent <- pagureListProjectIssueTitlesStatus "pagure.io"
                     "releng/fedora-scm-requests"
                     [makeItem "author" fasid, makeItem "status" "all",
                      makeItem "per_page" "100"]
          case erecent of
            Left err -> error' err
            Right recent -> filterM (notExistingRequest recent) newbranches

    -- FIXME handle close_status Invalid
    notExistingRequest :: [IssueTitleStatus] -> Branch -> IO Bool
    notExistingRequest requests br = do
      let pending = filter ((("New Branch \"" ++ show br ++ "\" for \"rpms/" ++ unPackage pkg ++ "\"") ==) . pagureIssueTitle) requests
      unless (null pending) $ do
        putStrLn $ "Branch request already exists for" +-+ unPackage pkg ++ ":" ++ show br
        mapM_ printScmIssue pending
      return $ null pending

havePkgAccess :: Package -> IO Bool
havePkgAccess pkg = do
  -- check have access
  fasid <- fasIdFromKrb
  epkginfo <- pagureProjectInfo srcfpo ("rpms" </> unPackage pkg)
  case epkginfo of
    Left err -> error' err
    Right pkginfo -> do
      let (admins, committers) = usersWithAccess pkginfo :: ([String],[String])
          access = fasid `elem` admins ++ committers
      unless access $
        warning $ "-" +-+ fasid +-+ "does not have access, ask:" +-+ unwords admins
      return access
  where
    usersWithAccess pkginfo =
      let access = lookupKey' "access_users" pkginfo
          owners = lookupKey' "owner" access
          admins = lookupKey' "admin" access
          collabs = lookupKey' "collaborator" access
      in (owners ++ admins, collabs)
