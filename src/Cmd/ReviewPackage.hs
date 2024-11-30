{-# LANGUAGE CPP, OverloadedStrings #-}

module Cmd.ReviewPackage (
  reviewPackage
  ) where

import Common
import Common.System

import Data.Char
import Data.Tuple.Extra (second)
import Safe (headDef, headMay, tailSafe)
import SelectRPMs (selectDefault)
import SimplePrompt (promptEnter, yesNoDefault)

import Branches
import Bugzilla
import Cmd.Import (downloadReviewSRPM, upstreamDir)
import Cmd.Install (installCmd)
import Cmd.Local (localCmd)
import Git (isGitRepo, git_, gitBool)
import Package
import RpmBuild

-- FIXME does not work with pkg dir/spec: 'fbrnch: No spec file found'
-- FIXME --user to download all user's review requests
reviewPackage :: Bool -> Maybe String -> IO ()
reviewPackage interactive Nothing = do
  -- FIXME catch no spec file
  spec <- findSpecfile
  srpm <- generateSrpm Nothing spec
  if interactive
    then doInteractiveReview False (Just spec) srpm
    else do
    cmd_ "fedora-review" ["-rn", srpm]
reviewPackage interactive (Just pkgbug) = do
  let epkgbid =
        if all isDigit pkgbug
        then Right $ read pkgbug
        else Left pkgbug
  (bugs,session) <- bugsSession $
          case epkgbid of
            Right bid -> packageReview .&&. statusNewAssigned .&&. bugIdIs bid
            Left pkg -> pkgReviews pkg .&&. statusNewAssigned
  case bugs of
    [bug] -> do
      putReviewBug False bug
      let bid = bugId bug
          pkg = reviewBugToPackage bug
      if interactive
        then reviewPackageInteractive bid pkg session bug
        else do
        promptEnter "Press Enter to run fedora-review"
        -- FIXME support copr build
        -- FIXME if toolbox set REVIEW_NO_MOCKGROUP_CHECK
        cmd_ "fedora-review" ["-b", show bid]
    [] -> error' $ "No package review found for" +-+ pkgbug
    _ -> error' $ "More than one review bug found for" +-+ pkgbug

reviewPackageInteractive :: Int -> String -> BugzillaSession -> Bug -> IO ()
reviewPackageInteractive bid pkg session bug = do
  let dir = show bid ++ '-' : pkg
  -- FIXME check if current directory
  exists <- doesDirectoryExist dir
  unless exists $
    createDirectory dir
  setCurrentDirectory dir
  srpm <- downloadReviewSRPM True False pkg bid session
  importsrpm <-
    if exists
    then do
    putStrLn $ "in" +-+ dir </> ":\n"
    cmd_ "ls" ["-F"]
    putNewLn
    -- FIXME default to no if nvr unchanged?
    yesNoDefault True "Press Enter to install/prep srpm"
    else return True
  -- review and package name may be different (eg ramalama)
  doInteractiveReview importsrpm Nothing srpm
  putNewLn
  putReviewBug False bug
  putNewLn
  putStrLn $ "Review dir is" +-+ dir

doInteractiveReview :: Bool -> Maybe FilePath -> FilePath -> IO ()
doInteractiveReview importsrpm mspec srpm = do
  when importsrpm $ do
    isgit <- isGitRepo
    unless isgit $ git_ "init" []
    -- FIXME override %_sourcedir so it doesn't put elsewhere?
    sourcediropt <- sourceDirCwdOpt
    putStrLn "installing srpm:"
    cmd_ "rpm" $ ["-i", srpm] ++ sourcediropt
    spec <- maybe findSpecfile return mspec
    git_ "add" [spec]
    allsrcs <- map sourceFieldFile <$> cmdLines "spectool" [spec]
    forM_ allsrcs $ \src ->
      unless (isArchiveFile src) $
      git_ "add" [src]
    putStrLn $ "# Diff with" +-+ upstreamDir
    cmd_ "diff" ["-u", spec, upstreamDir </> spec]
    withCurrentDirectory upstreamDir $
      void $ getSources spec
    diff <- lines <$> cmdIgnoreErr "diff" ["--brief", ".", upstreamDir] ""
    let filterdiff = filter (\d -> not (any (`isSuffixOf` d) ["SPEC","SRPMS","RPMS","BUILD","BUILDROOT","src.rpm",".log", ".git", upstreamDir])) diff
    if null filterdiff
      then putStrLn $ "no difference with" +-+ upstreamDir ++ "/"
      else mapM_ putStrLn filterdiff
    unlessM (gitBool "diff" ["--quiet", "--cached"]) $
      git_ "commit" ["-m", srpm]
  putNewLn
  putStrLn "# Build"
  -- FIXME or download rpms
  build <- yesNoDefault importsrpm "Build package locally"
  when build $
    localCmd False False Nothing [] (Branches [],[])
  putNewLn
  putStrLn "# RpmLint"
  void $ cmdBool "rpmlint" ["."] -- FIXME $ spec:srpm:rpms
  spec <- maybe findSpecfile return mspec
  whenM (yesNoDefault importsrpm "Install packages locally") $ do
    installCmd False False Nothing Nothing [] False True True selectDefault Nothing (Nothing,[])
    rpms <- cmdLines "rpmspec" ["-q", "--rpms", "--qf", "%{name}\n", spec]
    whenM (yesNoDefault importsrpm "Rpmlint installed packages") $ do
       (_ok, out, err) <- cmdFull "rpmlint" ("-i" : rpms) ""
       let rpmlintout = "rpmlint.output"
       writeFile rpmlintout out
       let ls = lines out
           nolines = length ls
       if nolines > 20
         then do
         mapM_ putStrLn $ takeEnd 10 ls
         putNewLn
         putStrLn "RpmLint summary:"
         mapM_ (putStrLn . renderLintSummary) $ summarizeErrors ls
         putStrLn $ show nolines +-+ "lines saved to" +-+ rpmlintout
         else putStrLn out
       unless (null err) $ warning $ "rpmlint stderr:\n" ++ err
       putNewLn
  putStrLn "# Licensing"
  -- FIXME use build subdir
  -- FIXME filter out files not in tarball or prep
  cmdLines "licensecheck" ["-r", "BUILD"] >>=
    -- handle "FILEPATH: *No copyright* UNKNOWN [generated file]"
    mapM_ putStrLn . filter (not . (" UNKNOWN" `isInfixOf`))
  cmd_ "rpmspec" ["-q", "--srpm", "--qf", "Spec license: %{license}\n", spec]

summarizeErrors :: [String] -> [(String,Int)]
summarizeErrors =
  sortOn fst . map (second length) . groupOnKey (headDef "''") . map tailSafe . filter ((Just "E:" ==) . headMay) . map (tailSafe . words)

renderLintSummary :: (String,Int) -> String
renderLintSummary (err,n) = err ++ ":" +-+ show n

#if !MIN_VERSION_extra(1,7,11)
groupOnKey :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOnKey _ []     = []
groupOnKey f (x:xs) = (fx, x:yes) : groupOnKey f no
    where
        fx = f x
        (yes, no) = span (\y -> fx == f y) xs
#endif
