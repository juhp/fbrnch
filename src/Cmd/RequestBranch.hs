module Cmd.RequestBranch (
  BranchesRequest(..),
  requestBranches
  ) where

import Common
import Common.System
import qualified Common.Text as T

import Branches
import Bugzilla
import Git
import Krb
import ListReviews
import Package
import Pagure
import Prompt

data BranchesRequest = AllReleases | BranchesRequest [Branch]

requestBranches :: Bool -> BranchesRequest -> IO ()
requestBranches mock request =
  ifM isPkgGitDir
    (getDirectoryName >>= requestPkgBranches mock request . Package) $
    do pkgs <- map reviewBugToPackage <$> listReviews ReviewUnbranched
       mapM_ (\ p -> withExistingDirectory p $ requestPkgBranches mock request (Package p)) pkgs

-- FIXME add --yes, or skip prompt when args given
requestPkgBranches :: Bool -> BranchesRequest -> Package -> IO ()
requestPkgBranches mock request pkg = do
  putPkgHdr pkg
  git_ "fetch" []
  active <- getFedoraBranched
  branches <- do
    let requested = case request of
                      AllReleases -> active
                      BranchesRequest [] -> take 2 active
                      BranchesRequest brs -> brs
    inp <- prompt $ "Confirm branches [" ++ unwords (map show requested) ++ "]"
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
      existing <- fedoraBranchesNoMaster localBranches
      forM_ brs $ \ br ->
        when (br `elem` existing) $
        putStrLn $ show br ++ " branch already exists"
      let brs' = brs \\ existing
      if null brs' then return []
        else do
        current <- fedoraBranchesNoMaster $ pagurePkgBranches (unpackage pkg)
        forM_ brs' $ \ br ->
          when (br `elem` current) $
          putStrLn $ show br ++ " remote branch already exists"
        let newbranches = brs' \\ current
        if null newbranches then return []
          else do
          fasid <- fasIdFromKrb
          erecent <- pagureListProjectIssueTitles "pagure.io" "releng/fedora-scm-requests"
                     [makeItem "author" fasid, makeItem "status" "all"]
          case erecent of
            Left err -> error' err
            Right recent -> filterM (notExistingRequest recent) newbranches

    notExistingRequest :: [(Integer,String,T.Text)] -> Branch -> IO Bool
    notExistingRequest requests br = do
      let pending = filter ((("New Branch \"" ++ show br ++ "\" for \"rpms/" ++ unpackage pkg ++ "\"") ==) . snd3) requests
      unless (null pending) $ do
        putStrLn $ "Branch request already open for " ++ unpackage pkg ++ ":" ++ show br
        mapM_ printScmIssue pending
      return $ null pending

    mockConfig :: Branch -> String
    mockConfig Master = "fedora-rawhide-x86_64"
    mockConfig (Fedora n) = "fedora-" ++ show n ++ "-x86_64"
    mockConfig (EPEL n) = "epel-" ++ show n ++ "-x86_64"
