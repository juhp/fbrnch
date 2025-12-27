{-# LANGUAGE OverloadedStrings #-}

module Cmd.RequestRepo (requestRepos) where

import Control.Exception.Extra (retry)
import Fedora.Krb (fasIdFromKrb)
import Network.HTTP.Directory (httpExists')
import Safe (headMay)
import SimplePrompt (promptEnter, promptInitial, yesNo)
import System.Time.Extra (sleep)

import Branches
import Bugzilla
import Cmd.Import (importCmd)
import Common
import Common.System (error')
import qualified Common.Text as T
import ListReviews
import Package
import Pagure

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
        when (resubmit && pagureIssueCloseStatus request == Just "Processed") $
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
        fedpkg_ "request-repo" [pkg, show bid]
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
      erecent <-
        retry 2 $
        pagureListProjectIssueTitlesStatus pagureio
        "releng/fedora-scm-requests"
        [makeItem "author" fasid, makeItem "status" "all",
         makeItem "per_page" "100"]
      case erecent of
        Left err -> error' err
        Right recent ->
          -- don't mention "New Repo" here, since Branch requests also imply repo already exists
          return $ filter (((" for \"rpms/" ++ pkg ++ "\"") `isSuffixOf`) . pagureIssueTitle) recent

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
