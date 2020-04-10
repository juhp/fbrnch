module Cmd.Import (importCmd) where

import Control.Monad
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.URI
import SimpleCmd
import System.Directory
import System.FilePath
import Web.Bugzilla (Comment, commentText, getComments)

import Bugzilla
import Git
import Krb
import ListReviews
import Package
import Prompt
import Types (Package)

-- FIXME separate pre-checked listReviews and direct pkg call, which needs checks
importCmd :: [Package] -> IO ()
importCmd ps = do
  pkgs <- if null ps
    then map reviewBugToPackage <$> listReviews ReviewRepoCreated
    else return ps
  mapM_ importPkg pkgs

-- FIXME check not in a different git dir
importPkg :: String -> IO ()
importPkg pkg = do
  putPkgHdr pkg
  dir <- getCurrentDirectory
  when (pkg /= takeFileName dir) $ do
    direxists <- doesDirectoryExist pkg
    -- FIXME check repo exists
    unless direxists $ fedpkg_ "clone" [pkg]
    setCurrentDirectory pkg
    -- FIXME: check branch is master
  isGit <- doesDirectoryExist ".git"
  unless isGit $ error' "Not a git repo"
  newrepo <- initialPkgRepo
  if not newrepo
    then putStrLn "Skipping: already imported"
    else do
    checkWorkingDirClean
    -- FIXME get session from importPkgs
    (bid,session) <- approvedReviewBugIdSession pkg
    comments <- getComments session bid
    putStrLn ""
    putBugId bid
    mapM_ showComment comments
    prompt_ "to continue"
    let srpms = map (T.replace "/reviews//" "/reviews/") $ concatMap findSRPMs comments
    when (null srpms) $ error "No srpm urls found!"
    mapM_ T.putStrLn srpms
    let srpm = (head . filter isURI . filter (".src.rpm" `isSuffixOf`) . words . T.unpack . last) srpms
    let srpmfile = takeFileName srpm
    -- FIXME if havesrpm then print local filename
    prompt_ $ "to import " ++ srpmfile
    havesrpm <- doesFileExist srpmfile
    unless havesrpm $
      cmd_ "curl" ["--silent", "--show-error", "--remote-name", srpm]
    krbTicket
    fedpkg_ "import" [srpmfile]
    git_ "commit" ["--message", "import #" ++ show bid]
    nvr <- fedpkg "verrel" []
    prompt_ $ "to push and build " ++ nvr
    gitPushSilent
    fedpkg_ "build" ["--fail-fast"]
    postBuildComment session nvr bid
    -- FIXME build branches too
  when (pkg /= takeFileName dir) $
    setCurrentDirectory dir
  where
    findSRPMs :: Comment -> [T.Text]
    findSRPMs =
      filter (\ l -> "https://" `T.isInfixOf` l && any (`T.isPrefixOf` T.toLower l) ["srpm url:", "srpm:", "new srpm:", "updated srpm:"] && ".src.rpm" `T.isSuffixOf` l) . T.lines . commentText
