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
requestRepos :: [String] -> IO ()
requestRepos ps = do
  pkgs <- if null ps
    then map reviewBugToPackage <$> listReviews ReviewWithoutRepoReq
    else return ps
  mapM_ requestRepo pkgs

-- FIXME also accept bugid instead
requestRepo :: String -> IO ()
requestRepo pkg = do
  putStrLn pkg
  (bid,session) <- approvedReviewBugIdSession pkg
  putBugId bid
  created <- checkRepoCreatedComment session bid
  if created
    then putStrLn "scm repo was already created"
    else do
    -- show comments?
    requestExists <- openRepoRequest
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
    openRepoRequest :: IO Bool
    openRepoRequest = do
      fasid <- fasIdFromKrb
      erecent <- pagureListProjectIssueTitles pagureio "releng/fedora-scm-requests" [makeItem "author" fasid, makeItem "status" "all"]
      case erecent of
        Left err -> error' err
        Right recent -> do
          -- don't mention "New Repo" here, since Branch requests also imply repo already exists
          let reqs = filter (((" for \"rpms/" ++ pkg ++ "\"") `isSuffixOf`) . snd3) recent
          unless (null reqs) $ do
            -- FIXME improve formatting (reduce whitespace)
            putStrLn "Request exists:"
            mapM_ printScmIssue reqs
          return $ not (null reqs)

    checkNoPagureRepo :: IO ()
    checkNoPagureRepo = do
      res <- pagureProjectInfo srcfpo $ "rpms" </> pkg
      when (isRight res) $
        error' $ "Repo for " ++ pkg ++ " already exists"
