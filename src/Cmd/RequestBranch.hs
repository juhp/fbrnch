module Cmd.RequestBranch (
  requestBranches,
  requestPkgBranches
  ) where

import Common
import Common.System

import Branches
import Bugzilla
import Git
import Krb
import ListReviews
import Package
import Pagure

-- FIXME option to do koji scratch build instead of mock
requestBranches :: Bool -> (BranchesReq,[String]) -> IO ()
requestBranches mock (breq, ps) = do
  if null ps
    then do
    isPkgGit <- isPkgGitSshRepo
    if isPkgGit
      then getDirectoryName >>= requestPkgBranches False mock breq . Package
      else do
      pkgs <- map reviewBugToPackage <$> listReviews ReviewUnbranched
      mapM_ (\ p -> withExistingDirectory p $ requestPkgBranches (length pkgs > 1) mock breq (Package p)) pkgs
    else
    forM_ ps $ \ p ->
    withExistingDirectory p $ do
    pkg <- getDirectoryName
    requestPkgBranches (length ps > 1) mock breq (Package pkg)

requestPkgBranches :: Bool -> Bool -> BranchesReq -> Package -> IO ()
requestPkgBranches multiple mock breq pkg = do
  when (breq == Branches []) $
    putPkgHdr pkg
  git_ "fetch" []
  branches <- getRequestedBranches breq
  newbranches <- filterExistingBranchRequests branches
  unless (null newbranches) $ do
    mbidsession <- bzReviewSession
    urls <- forM newbranches $ \ br -> do
      when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br]
      when multiple $ putStr (unPackage pkg ++ " ")
      when (length newbranches > 1) $ print br
      -- Can timeout like this:
      -- Could not execute request_branch: HTTPSConnectionPool(host='pagure.io', port=443): Read timed out. (read timeout=60)
      -- fbrnch: readCreateProcess: fedpkg "request-branch" "epel9" (exit 1): failed
      fedpkg "request-branch" [show br]
    putStrLn $ unlines urls
    whenJust mbidsession $ \(bid,session) ->
      commentBug session bid $ unlines urls
  where
    filterExistingBranchRequests :: [Branch] -> IO [Branch]
    filterExistingBranchRequests branches = do
      existing <- fedoraBranchesNoRawhide (localBranches True)
      forM_ branches $ \ br ->
        when (br `elem` existing) $
        putStrLn $ show br ++ " branch already exists"
      let brs' = branches \\ existing
      if null brs'
        then return []
        else do
        current <- fedoraBranchesNoRawhide $ pagurePkgBranches (unPackage pkg)
        forM_ brs' $ \ br ->
          when (br `elem` current) $
          putStrLn $ show br ++ " remote branch already exists"
        let newbranches = brs' \\ current
        if null newbranches then return []
          else do
          fasid <- fasIdFromKrb
          erecent <- pagureListProjectIssueTitlesStatus "pagure.io" "releng/fedora-scm-requests"
                     [makeItem "author" fasid, makeItem "status" "all"]
          case erecent of
            Left err -> error' err
            Right recent -> filterM (notExistingRequest recent) newbranches

    -- FIXME handle close_status Invalid
    notExistingRequest :: [IssueTitleStatus] -> Branch -> IO Bool
    notExistingRequest requests br = do
      let pending = filter ((("New Branch \"" ++ show br ++ "\" for \"rpms/" ++ unPackage pkg ++ "\"") ==) . pagureIssueTitle) requests
      unless (null pending) $ do
        putStrLn $ "Branch request already open for " ++ unPackage pkg ++ ":" ++ show br
        mapM_ printScmIssue pending
      return $ null pending
