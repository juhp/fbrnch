{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmd.RequestRepo (requestRepos) where

import Control.Exception.Extra (retry)
import qualified Data.ByteString.Lazy.Char8 as B
import Fedora.Krb (fasIdFromKrb)
import Gitea.Forgejo (fedoraForge, listRepoIssues, RepoIssueOptions(..))
import Network.HTTP.Directory (httpExists')
import Safe (headMay)
import SimplePrompt (promptEnter, promptInitial, yesNo)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import qualified System.Process.Typed as P
#if !MIN_VERSION_base(4,14,0)
import qualified System.Exit as P
#endif
import System.Time.Extra (sleep)

import Branches
import Bugzilla
import Cmd.Import (importCmd)
import Common
import Common.System (error')
import qualified Common.Text as T
import Forgejo
import ListReviews
import Package
import Pagure (pagureListGitBranches, srcfpo)

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
requestRepos :: Bool -> Bool -> Bool -> Bool -> (BranchesReq, [String])
             -> IO ()
requestRepos mock allstates skipcheck resubmit (breq, ps) = do
  when (resubmit && length ps /= 1) $
    error' "can only --resubmit for a single package"
  pkgs <- if null ps
    then map reviewBugToPackage <$> listReviewsAll allstates ReviewWithoutRepoReq
    else return ps
  if null pkgs
    then error' "No approved package reviews found"
    else mapM_ (requestRepo mock skipcheck resubmit breq) pkgs

-- FIXME also accept bugid instead
requestRepo :: Bool -> Bool -> Bool -> BranchesReq -> String -> IO ()
requestRepo mock skipcheck resubmit breq pkg = do
  putStrLn pkg
  (bug,session) <- approvedReviewBugSession pkg
  putBug bug
  let bid = bugId bug
  if bugAssignedTo bug == "nobody@fedoraproject.org" then
    putStrLn "Review bug needs to be assigned to reviewer first"
    else do
    created <- checkRepoCreatedComment session bid
    if created
      then putStrLn "scm repo was already created"
      else do
      requests <-
        if skipcheck then return [] else existingRepoRequests
      whenJust (headMay requests) $ \request -> do
        putStrLn "Request exists:"
        mapM_ printScmIssue requests
        -- don't resubmit if succeeded
        when (resubmit && hasIssueLabel "Processed" request) $
          error' "The last repo request was already successfully Processed"
      when (null requests || resubmit) $ do
        checkNoPagureRepo
        -- may be truncated with "<rest of output cut here>"
        comments <- getComments session bid
        mapM_ showComment comments
        putNewLn
        putBugId $ bugId bug
        putNewLn
        promptEnter "Press Enter to continue"
        -- FIXME check api key is still valid or open pagure ticket directly
        doRequestRepo pkg bid
        putNewLn
        let draft = "Thank you for the review" ++ maybe "" ("," +-+) (assigneeFirstname $ bugAssignedToDetail bug)
        input <- promptInitial "Enter comment" draft
        unless (null input) $
          commentBug session bid input
        putNewLn
        branches <- getRequestedBranches [] breq
        forM_ branches $ \ br -> do
          when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br Nothing]
          putStr $ showBranch br ++ " "
          fedpkg_ "request-branch" ["--repo", pkg, showBranch br]
        putNewLn
        ok <- yesNo $ "Import" +-+ pkg
        when ok $ do
          waitForPagureRepo
          importCmd False True False False (Branches [],[pkg])
  where
    existingRepoRequests :: IO [IssueTitleStatus]
    existingRepoRequests = do
      fasid <- fasIdFromKrb
      eissues <-
        retry 2 $
        listRepoIssues fedoraForge "releng" "fedora-scm-requests" $
        RepoIssueOptions (Just fasid) (Just "all")
      case eissues of
        Left err -> error' err
        Right issues -> do
          let its = mapMaybe readIssue issues
          -- don't mention "New Repo" here, since Branch requests also imply repo already exists
          return $ filter (((" for \"rpms/" ++ pkg ++ "\"") `isSuffixOf`) . forgeIssueTitle) its

    checkNoPagureRepo :: IO ()
    checkNoPagureRepo = do
      exists <- httpExists' $ "https://" ++ srcfpo +/+ "rpms" +/+ pkg
      when exists $
        error' $ "Repo for" +-+ pkg +-+ "already exists"

    -- FIXME handle "email name"
    assigneeFirstname :: User -> Maybe String
    assigneeFirstname assignee =
      case T.words $ userRealName assignee of
          [] -> Nothing
          first:_ ->
            if "@" `T.isInfixOf` first
            then Nothing
            else Just (T.unpack first)

    waitForPagureRepo :: IO ()
    waitForPagureRepo = do
      ebrs <- pagureListGitBranches srcfpo $ "rpms/" ++ pkg
      case ebrs of
        Left _err -> do
          putChar '.'
          sleep 10
          waitForPagureRepo
        Right brs ->
          when (null brs) $
          error' $ "no branches in dist-git for" +-+ pkg

doRequestRepo :: String -> Int -> IO ()
doRequestRepo pkg bid = do
  file <- getUserConfigFile "rpkg" "fedpkg.conf"
  cfg <- doesFileExist file
  if cfg
    then do
    (exit, err) <- P.readProcessStderr $ P.proc "fedpkg" ["request-repo", pkg, show bid]
    let errstr = B.unpack err
    unless (exit == P.ExitSuccess) $ do
      if "Invalid or expired token." `isInfixOf` errstr
        then do
        putStrLn "pagure.io token expired or invalid"
        putStrLn "Refresh token at https://pagure.io/settings#nav-api-tab and paste here directly into 'fedpkg set-pagure-token' now"
        setPagureToken
        else error' errstr
    else do
    putStrLn "Missing pagure.io token"
    putStrLn "Please create a token for issue creation at https://pagure.io/settings#nav-api-tab and paste here into 'fedpkg set-pagure-token' now"
    setPagureToken
  where
    setPagureToken = do
      ret <- P.runProcess $ P.proc "fedpkg" ["set-forgejo-token"]
      if ret == P.ExitSuccess
        then doRequestRepo pkg bid
        else error' "Failed to set token"
