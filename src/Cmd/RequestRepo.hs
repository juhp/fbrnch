{-# LANGUAGE OverloadedStrings #-}

module Cmd.RequestRepo (requestRepos) where

import Network.HTTP.Directory (httpExists, httpManager)
import SimpleCmd
import SimplePrompt (promptEnter, promptInitial)

import Branches
import Bugzilla
import Common
import qualified Common.Text as T
import Krb
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
  mapM_ (requestRepo mock skipcheck resubmit breq) pkgs

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
      unless (null requests) $ do
        putStrLn "Request exists:"
        mapM_ printScmIssue requests
        -- don't resubmit if succeeded
        -- FIXME head
        when (resubmit && pagureIssueCloseStatus (head requests) == Just "Processed") $
          error' "The last repo request was already successfully Processed"
      when (null requests || resubmit) $ do
        checkNoPagureRepo
        putNewLn
        comments <- getComments session bid
        mapM_ showComment comments
        putNewLn
        putBugId $ bugId bug
        putNewLn
        promptEnter "Press Enter to continue"
        -- FIXME check api key is still valid or open pagure ticket directly
        fedpkg_ "request-repo" [pkg, show bid]
        let draft = "Thank you for the review" ++ maybe "" ("," +-+) (assigneeFirstname $ bugAssignedToDetail bug)
        putStrLn "```"
        putStrLn draft
        putStrLn "```"
        input <- promptInitial "Enter comment" draft
        let comment = (if null input then draft else input)
        commentBug session bid comment
        putNewLn
        branches <- getRequestedBranches [] breq
        forM_ branches $ \ br -> do
          when mock $ fedpkg_ "mockbuild" ["--root", mockRoot br Nothing]
          putStr $ show br ++ " "
          fedpkg_ "request-branch" ["--repo", pkg, show br]
        putNewLn
  where
    existingRepoRequests :: IO [IssueTitleStatus]
    existingRepoRequests = do
      fasid <- fasIdFromKrb
      erecent <-
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
      mgr <- httpManager
      exists <- httpExists mgr $ "https://" ++ srcfpo +/+ "rpms" +/+ pkg
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
