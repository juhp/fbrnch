module Cmd.RequestBranch (
  BranchesRequest(..),
  requestBranches
  ) where

import Common

import SimpleCmd

import Branches
import Bugzilla
import Git
import Krb
import ListReviews
import Package
import Prompt

data BranchesRequest = AllReleases | BranchesRequest [Branch]

-- FIXME if pkg dir than just act on package
requestBranches :: Bool -> BranchesRequest -> IO ()
requestBranches mock request = do
  pkgs <- map reviewBugToPackage <$> listReviews ReviewUnbranched
  mapM_ (\ p -> withExistingDirectory p $ requestPkgBranches mock request p) pkgs

requestPkgBranches :: Bool -> BranchesRequest -> String -> IO ()
requestPkgBranches mock request pkg = do
  putPkgHdr pkg
  gitPull
  active <- getFedoraBranched
  branches <- do
    let requested = case request of
                      AllReleases -> active
                      BranchesRequest [] -> take 2 active
                      BranchesRequest brs -> brs
    inp <- prompt $ "Enter branches [" ++ unwords (map show requested) ++ "]"
    return $ if null inp
             then requested
             else map (readActiveBranch' active) $ words inp
  newbranches <- filterExistingBranchRequests branches
  forM_ newbranches $ \ br -> do
    when mock $ fedpkg_ "mockbuild" ["--root", mockConfig br]
    fedpkg_ "request-branch" [show br]
  where
    filterExistingBranchRequests :: [Branch] -> IO [Branch]
    filterExistingBranchRequests brs = do
      existing <- packageBranched
      forM_ brs $ \ br ->
        when (br `elem` existing) $
        putStrLn $ show br ++ " branch already exists"
      let brs' = brs \\ existing
      if null brs' then return []
        else do
        current <- packagePagureBranched pkg
        forM_ brs' $ \ br ->
          when (br `elem` current) $
          putStrLn $ show br ++ " remote branch already exists"
        let newbranches = brs' \\ current
        if null newbranches then return []
          else do
          fasid <- fasIdFromKrb
          -- FIXME use rest api
          open <- cmdLines "pagure" ["issues", "--server", "pagure.io", "--author", fasid, "releng/fedora-scm-requests"]
          filterM (notExistingRequest open) newbranches

    notExistingRequest :: [String] -> Branch -> IO Bool
    notExistingRequest requests br = do
      let pending = filter (("New Branch \"" ++ show br ++ "\" for \"rpms/" ++ pkg ++ "\"") `isInfixOf`) requests
      unless (null pending) $
        putStrLn $ "Branch request already open for " ++ pkg ++ ":" ++ show br ++ "\n" ++ unlines pending
      return $ null pending

    mockConfig :: Branch -> String
    mockConfig Master = "fedora-rawhide-x86_64"
    mockConfig (Fedora n) = "fedora-" ++ show n ++ "-x86_64"
    mockConfig (EPEL n) = "epel-" ++ show n ++ "-x86_64"
