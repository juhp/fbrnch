module Cmd.RequestRepo (requestRepos) where

import Common
import Network.HTTP.Simple
import SimpleCmd

import Bugzilla
import Krb
import ListReviews
import Package

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
      -- FIXME use rest api
      fasid <- fasIdFromKrb
      current <- cmdLines "pagure" ["issues", "--server", "pagure.io", "--all", "--author", fasid, "releng/fedora-scm-requests"]
      -- don't mention "New Repo" here:
      -- pending Branch requests imply repo already exists
      let reqs = filter ((" for \"rpms/" ++ pkg ++ "\"") `isInfixOf`) current
      unless (null reqs) $
        -- FIXME improve formatting (reduce whitespace)
        putStrLn $ "Request exists:\n" ++ unlines reqs
      return $ not (null reqs)

    checkNoPagureRepo :: IO ()
    checkNoPagureRepo = do
      out <- cmd "pagure" ["list", "--namespace", "rpms", pkg]
      unless (null out) $
        error' $ "Repo for " ++ pkg ++ " already exists"
