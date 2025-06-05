{-# LANGUAGE CPP, OverloadedStrings #-}

module Cmd.RequestBranch (
  requestBranchesCmd,
  requestPkgBranches
  ) where

import Fedora.Krb (fasIdFromKrb)
import Network.HTTP.Query (lookupKey')
import SimplePrompt (promptEnter)
import System.Time.Extra (sleep)

import Common
import Common.System

import Branches
import Bugzilla
import Cmd.SrcDeps (srcDeps)
import Git
import Koji (fedoraHub, kojiBuildTarget')
import ListReviews
import Package
import Pagure

-- FIXME try to catch expired token (maybe record date when key updated?)
-- request-branch and request-repo require pagure.io API key in ~/.config/rpkg/fedpkg.conf for opening releng-scm issue: following is error for expired key

-- "Could not execute request_branch: The following error occurred while creating a new issue in Pagure: Invalid or expired token. Please visit https://pagure.io/settings#nav-api-tab to get or renew your API token.
-- For invalid or expired tokens please set a new token in your user configuration by running:
--         fedpkg set-pagure-token
-- The command is interactive; enter the new token when prompted."

-- FIXME option to do koji scratch build instead of mock
requestBranchesCmd :: Bool -> Bool -> Bool -> Maybe Branch -> Bool
                   -> (BranchesReq,[String]) -> IO ()
requestBranchesCmd quiet reviews skipcheck mrecursebr mock (breq, ps) = do
  if null ps
    then do
    when (isJust mrecursebr) $
      error' "please specify a package dir when using --recurse-deps"
    isPkgGit <- isPkgGitSshRepo
    if reviews
      then do
      pkgs <- map reviewBugToPackage <$> listReviews ReviewUnbranched
      when isPkgGit $
        promptEnter "Press Enter to continue inside this dist-git dir!"
      mapM_ (\ p -> withExistingDirectory p $ requestPkgBranches quiet (length pkgs > 1) mock breq (Package $ takeFileName p)) pkgs
      else
      if isPkgGit
      then do
        pkg <- Package <$> getDirectoryName
        requestPkgBranches quiet False mock breq pkg >>=
          mapM_ (waitForKojiPkgBranch skipcheck pkg)
      else error' "not a dist-git dir: specify package(s)"
    else do
    pkgs <-
      case mrecursebr of
        Just br -> do
          -- FIXME --rpmopt
          deps <- concat <$> srcDeps False [] (br,ps)
          when (null deps) $
            error' "no deps found!"
          putStrLn $ unwords deps
          unless quiet $
            promptEnter "\nPress Enter to check these packages for branches"
          return deps
        Nothing -> return ps
    newPkgBranches <-
      forM pkgs $ \ p ->
      withExistingDirectory p $ do
      pkg <- Package <$> getDirectoryName
      requestPkgBranches quiet (length pkgs > 1) mock breq pkg
    putNewLn
    forM_ (zip pkgs newPkgBranches) $ \(pkg,newbrs) ->
      forM_ newbrs $ waitForKojiPkgBranch skipcheck (Package $ takeFileName pkg)

requestPkgBranches :: Bool -> Bool -> Bool -> BranchesReq -> Package
                   -> IO [Branch]
requestPkgBranches quiet multiple mock breq pkg = do
  when (multiple && not quiet) $
    putPkgHdr pkg
  brs <- localBranches False
  branches <- getRequestedBranches brs breq
  if null branches
    then do
    case breq of
      Branches [_] -> putStrLn "exists"
      _ -> putStrLn "branches exist"
    return []
    else do
    gitFetchSilent True
    brs' <- localBranches False
    forM_ branches $ \br ->
      when (showBranch br `elem` brs') $
      let pkgPrefix = if multiple then unPackage pkg ++ ": " else ""
      in putStrLn $ pkgPrefix ++ showBranch br +-+ "branch already exists locally"
    if null (map showBranch branches \\ brs')
      then return []
      else do
      branches' <- getRequestedBranches brs' (Branches branches)
      haveAccess <- havePkgAccess pkg
      if not haveAccess
        then do
        putStrLn "You do not have commit access to this package: request would be rejected"
        return []
        else do
        newbranches <- filterExistingBranchRequests branches'
        if null newbranches
          then return []
          else do
          mbidsession <- bzReviewSession
          urls <- forM newbranches $ \ br -> do
            when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br Nothing]
            when (length branches > 1) $ putStr $ showBranch br ++ " "
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
            u <- fedpkg "request-branch" [showBranch br]
            putStrLn u
            return u
          whenJust mbidsession $ \(bid,session) ->
            commentBug session bid $ unlines urls
          return newbranches
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
        putStrLn $ pkgPrefix ++ showBranch br +-+ "branch already exists"
      let brs' = branches \\ existing
      if null brs'
        then return []
        else do
        current <- fedoraBranchesNoRawhide $ pagurePkgBranches (unPackage pkg)
        forM_ brs' $ \ br ->
          when (br `elem` current) $
          putStrLn $ pkgPrefix ++ showBranch br +-+ "remote branch already exists"
        let newbranches = brs' \\ current
        if null newbranches
          then return []
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

    -- FIXME print invalid requests?
    notExistingRequest :: [IssueTitleStatus] -> Branch -> IO Bool
    notExistingRequest requests br = do
      let processed = filter processedIssueFilter requests
      unless (null processed) $ do
        putStrLn $ "Branch request already exists for" +-+ unPackage pkg ++ ":" ++ showBranch br
        mapM_ printScmIssue processed
      return $ null processed
      where
        processedIssueFilter issue =
          pagureIssueTitle issue == ("New Branch \"" ++ showBranch br ++ "\" for \"rpms/" ++ unPackage pkg ++ "\"")
          &&
          pagureIssueCloseStatus issue == Just "Processed"

havePkgAccess :: Package -> IO Bool
havePkgAccess pkg = do
  fasid <- fasIdFromKrb
  epkginfo <- pagureProjectInfo srcfpo ("rpms" </> unPackage pkg)
  case epkginfo of
    Left err -> error' err
    Right pkginfo -> do
      let (admins, committers) = usersWithAccess pkginfo :: ([String],[String])
          (gadmins, gcommitters) = groupsWithAccess pkginfo :: ([String],[String])
      access <- do
        if fasid `elem` admins ++ committers
          then return True
          else
          fmap or <$>
            forM (gadmins ++ gcommitters) $ \grp -> do
            egrpinfo <- pagureGroupInfo srcfpo grp []
            case egrpinfo of
              Left err -> error' err
              Right grpinfo ->
                return $ fasid `elem` (lookupKey' "members" grpinfo :: [String])
      unless access $
        warning $ "-" +-+ fasid +-+ "does not have access, ask:" +-+ unwords (nub $ admins ++ gadmins)
      return access
  where
    usersWithAccess pkginfo =
      let access = lookupKey' "access_users" pkginfo
          owners = lookupKey' "owner" access
          admins = lookupKey' "admin" access
          collabs = lookupKey' "collaborator" access
          commits = lookupKey' "commit" access
      in (owners ++ admins, commits ++ collabs)

    groupsWithAccess pkginfo =
      let access = lookupKey' "access_groups" pkginfo
          admins = lookupKey' "admin" access
          collabs = lookupKey' "collaborator" access
          commits = lookupKey' "commit" access
      in (admins, commits ++ collabs)

waitForKojiPkgBranch :: Bool -- skipcheck
                     -> Package -> Branch -> IO ()
waitForKojiPkgBranch skipcheck pkg br =
  unless skipcheck $ do
  putStrLn $ "waiting for" +-+ unPackage pkg +-+ "to be added to" +-+ showBranch br ++ "-build"
  (buildtag,_desttag) <- kojiBuildTarget' fedoraHub (showBranch br)
  waitBuildTag buildtag
  where
    waitBuildTag :: String -> IO ()
    waitBuildTag buildtag = do
      sleep 30 -- wait first to avoid "(undefined package)"
      ok <- cmdBool "koji" ["list-pkgs", "--quiet", "--package=" ++ unPackage pkg, "--tag=" ++ buildtag]
      unless ok $ waitBuildTag buildtag
