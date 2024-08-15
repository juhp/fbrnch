{-# LANGUAGE OverloadedStrings #-}

module Cmd.ReviewPackage (
  reviewPackage
  ) where

import Common
import Common.System

import Data.Char
import SimplePrompt (promptEnter, yesNoDefault)

import Branches
import Bugzilla
import Cmd.Import (downloadReviewSRPM, pkgreviewSpecDir)
import Cmd.Install (installCmd)
import Cmd.Local (localCmd)
import Cmd.Prep (prepCmd)
import Package
import RpmBuild

-- FIXME does not work with pkg dir/spec:
-- 'fbrnch: No spec file found'
reviewPackage :: Bool -> Maybe String -> IO ()
reviewPackage _ Nothing = do
  -- FIXME catch no spec file
  spec <- findSpecfile
  srpm <- generateSrpm Nothing spec
  cmd_ "fedora-review" ["-rn", srpm]
reviewPackage download (Just pkgbug) = do
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
      if download
        then do
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
          yesNoDefault False $ "Press Enter to install and prep" +-+ srpm
          else return True
        let spec = pkg <.> "spec"
        when importsrpm $ do
          -- FIXME override %_SOURCEDIR so it doesn't put elsewhere
          cmd_ "rpm" ["-ivh", srpm]
          putStrLn "diff with SPEC/"
          cmd_ "diff" ["-u", spec, pkgreviewSpecDir </> spec]
          prepCmd Nothing False False False (Nothing,[])
          withCurrentDirectory pkgreviewSpecDir $
            void $ getSources spec
          diff <- lines <$> cmdIgnoreErr "diff" ["--brief", ".", pkgreviewSpecDir] ""
          let filterdiff = filter (\d -> not (any (`isSuffixOf` d) ["SPEC","SRPMS","RPMS","BUILD","BUILDROOT","src.rpm",".log"])) diff
          if null filterdiff
            then putStrLn $ "no difference with" +-+ pkgreviewSpecDir ++ "/"
            else mapM_ putStrLn filterdiff
        -- FIXME or download rpms
        build <- yesNoDefault importsrpm "Build package locally"
        when build $
          localCmd False False Nothing [] (Branches [],[])
        void $ cmdBool "rpmlint" ["."] -- FIXME $ spec:srpm:rpms
        whenM (yesNoDefault importsrpm "Install packages locally") $ do
          installCmd False False Nothing Nothing [] False True True False (Nothing,[])
          rpms <- cmdLines "rpmspec" ["-q", "--rpms", "--qf", "%{name}\n", spec]
          whenM (yesNoDefault importsrpm "Rpmlint installed packages") $
            void $ cmdBool "rpmlint" $ "-i" : rpms
        -- FIXME filter out unknown
        cmdLines "licensecheck" ["-r", "BUILD"] >>=
          -- handle "FILEPATH: *No copyright* UNKNOWN [generated file]"
          mapM_ putStrLn . filter (not . (" UNKNOWN" `isInfixOf`))
        cmd_ "rpmspec" ["-q", "--srpm", "--qf", "Spec license: %{license}\n", spec]
        putNewLn
        putReviewBug False bug
        putStrLn $ "package is in" +-+ dir
        else do
        promptEnter "Press Enter to run fedora-review"
        -- FIXME support copr build
        -- FIXME if toolbox set REVIEW_NO_MOCKGROUP_CHECK
        cmd_ "fedora-review" ["-b", show bid]
    [] -> error' $ "No package review found for" +-+ pkgbug
    _ -> error' $ "More than one review bug found for" +-+ pkgbug
