module PkgReview (
  ScratchOption(..),
  buildAndUpload,
  mockRpmLint,
  uploadPkgFiles
  )
where

import Common
import Common.System

import Fedora.Krb (fasIdFromKrb)
import Network.HTTP.Directory (httpExists, httpManager)
import SimplePrompt (promptEnter, yesNoDefault)

import Branches
import Koji
import RpmBuild

data ScratchOption = ScratchBuild | ScratchTask Int | SkipScratch
  deriving Eq

buildAndUpload :: Maybe ScratchOption -> String -> String -> FilePath
               -> IO (Maybe String, String)
buildAndUpload mscratchOpt srpm pkg spec = do
  scratch <-
    if isNothing mscratchOpt
    then yesNoDefault False "Would you like to do a koji scratch build before submitting"
    else do
      let doscratch = mscratchOpt == Just ScratchBuild
      promptEnter $ "Press Enter to" +-+ if doscratch
                                         then "submit"
                                         else "upload"
      return doscratch
  mkojiurl <- case mscratchOpt of
                Just (ScratchTask tid) -> return $ Just ("https://koji.fedoraproject.org/koji/taskinfo?taskID=" ++ show tid)
                _ ->
                  if scratch
                  then Just <$> kojiScratchBuild "rawhide" [] srpm
                  else return Nothing
  specSrpmUrls <- uploadPkgFiles pkg spec srpm
  return (mkojiurl, specSrpmUrls)

uploadPkgFiles :: String -> FilePath -> FilePath -> IO String
uploadPkgFiles pkg spec srpm = do
  fasid <- fasIdFromKrb
  -- read ~/.config/fedora-create-review
  let sshhost = "fedorapeople.org"
      sshpath = "public_html/reviews/" ++ pkg
  cmd_ "ssh" [fasid ++ "@" ++ sshhost, "mkdir", "-p", sshpath]
  cmd_ "scp" [spec, srpm, fasid ++ "@" ++ sshhost ++ ":" ++ sshpath]
  getCheckedFileUrls $ "https://" <> fasid <> ".fedorapeople.org" +/+ removePrefix "public_html/" sshpath
  where
    getCheckedFileUrls :: String -> IO String
    getCheckedFileUrls uploadurl = do
      let specUrl = uploadurl +/+ takeFileName spec
          srpmUrl = uploadurl +/+ takeFileName srpm
      mgr <- httpManager
      checkUrlOk mgr specUrl
      checkUrlOk mgr srpmUrl
      return $ "Spec URL: " <> specUrl <> "\nSRPM URL: " <> srpmUrl
      where
        checkUrlOk mgr url = do
          okay <- httpExists mgr url
          unless okay $ error' $ "Could not access:" +-+ url

mockRpmLint :: Bool -> String -> FilePath -> FilePath -> IO ()
mockRpmLint mock pkg spec srpm = do
  rpms <-
    if mock then do
      -- FIXME check that mock is installed
      let resultsdir = "results_" ++ pkg
      cmd_ "mock" ["--root", mockRoot Rawhide Nothing, "--resultdir=" ++ resultsdir, srpm]
      map (resultsdir </>) . filter ((== ".rpm") . takeExtension) <$> listDirectory resultsdir
    else
      builtRpms (RelBranch Rawhide) spec >>= filterM doesFileExist
--  print rpms
  -- FIXME parse # of errors/warnings
  void $ cmdBool "rpmlint" $ spec:srpm:rpms
