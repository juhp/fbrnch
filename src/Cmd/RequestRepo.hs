{-# LANGUAGE OverloadedStrings #-}

module Cmd.RequestRepo (requestRepos) where

import Common

import Data.Either
import Network.HTTP.Simple
import SimpleCmd
import System.FilePath

import Bugzilla
import Krb
import ListReviews
import Package
import Pagure

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
requestRepos :: Bool -> [String] -> IO ()
requestRepos retry ps = do
  when (retry && length ps /= 1) $
    error' "--retry only for a single package"
  pkgs <- if null ps
    then map reviewBugToPackage <$> listReviews ReviewWithoutRepoReq
    else return ps
  mapM_ (requestRepo retry) pkgs

-- FIXME also accept bugid instead
requestRepo :: Bool -> String -> IO ()
requestRepo retry pkg = do
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
      -- show comments?
      requestExists <- existingRepoRequest
      if requestExists then return ()
        else do
        checkNoPagureRepo
        url <- fedpkg "request-repo" [pkg, show bid]
        putStrLn url
        -- FIXME get name of reviewer from bug
        let comment = "Thank you for the review\n\n" <> url
            req = setRequestMethod "POST" $
                  setRequestCheckStatus $
                  newBzRequest session ["bug", intAsText bid, "comment"] [makeTextItem "comment" comment]
        void $ httpNoBody req
        putStrLn "comment posted"
        putStrLn ""
  where
    existingRepoRequest :: IO Bool
    existingRepoRequest = do
      fasid <- fasIdFromKrb
      erecent <- pagureListProjectIssueTitlesStatus pagureio "releng/fedora-scm-requests" [makeItem "author" fasid, makeItem "status" "all"]
      case erecent of
        Left err -> error' err
        Right recent -> do
          -- don't mention "New Repo" here, since Branch requests also imply repo already exists
          let reqs = filter (((" for \"rpms/" ++ pkg ++ "\"") `isSuffixOf`) . pagureIssueTitle) recent
          unless (null reqs) $ do
            -- FIXME improve formatting (reduce whitespace)
            putStrLn "Request exists:"
            mapM_ printScmIssue reqs
            when (retry && pagureIssueCloseStatus (head reqs) == Just "Processed") $
              error' "The last repo request was already successfully Processed"
          return $ notNull reqs && not retry

    checkNoPagureRepo :: IO ()
    checkNoPagureRepo = do
      res <- pagureProjectInfo srcfpo $ "rpms" </> pkg
      when (isRight res) $
        error' $ "Repo for " ++ pkg ++ " already exists"
