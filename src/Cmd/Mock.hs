module Cmd.Mock
  ( mockCmd,
    NoClean(..),
    MockMode(..)
  )
where

import Data.RPM
import Safe (headMay)
import SelectRPMs

import Branches
import Common
import Common.System
import Git
import Package
import RpmBuild (builtRpms, generateSrpm, nvraInstalled)

data NoClean = NoCleanBefore | NoCleanAfter | NoCleanAll | MockShortCircuit
  deriving Eq

-- FIXME (add MockBuild or MockDefault?)
data MockMode = ShellOnly | BuildShell | MockInstall
  deriving Eq

reqShell :: Maybe MockMode -> Bool
reqShell Nothing = False
reqShell (Just ms) = ms /= MockInstall

-- FIXME error if non-existent branch
-- FIXME add repo/copr for build
-- FIXME handle non-release branches (on-branch works)
-- FIXME option for --shell without rebuild
mockCmd :: Bool -> Maybe NoClean -> Bool -> Bool -> Maybe MockMode
        -> Maybe Branch -> Maybe String -> (BranchesReq, [String]) -> IO ()
mockCmd dryrun mnoclean network reinstall mockmode mroot march (breq, ps) = do
  pkggit <- isPkgGitRepo
  branches <-
    case breq of
      Branches [] ->
        pure <$>
        if null ps && pkggit
        then getReleaseBranch
        else systemBranch
      _ ->  listOfBranches False False breq
  when (null branches && length ps > 1 && isNothing mroot) $
    error' "Must specific branch or --root chroot"
  let packages = if null ps then ["."] else ps
  mapM_ (mockBuildPkgs pkggit (breq == Branches []) packages) branches
  where
    mockBuildPkgs :: Bool -> Bool -> [String] -> Branch -> IO ()
    mockBuildPkgs pkggit noswitch pkgs br = do
      srpms <- mapM (prepSrpm (RelBranch br)) pkgs
      putNewLn
      -- FIXME? is it better just to fail asking for target branch?
      rootBr <- maybe (if pkggit then getReleaseBranch else return br) return mroot
      let resultdir =
            case srpms of
              [] -> error' "cannot build zero packages"
              [srpm] ->
                let verrel = showPkgVerRel . readNVRA $ srpm
                 in ["--resultdir=results" </> verrel]
              _ -> []
      let command = if length pkgs > 1 then "--chain" else "--rebuild"
          noclean = case mnoclean of
                      Nothing -> []
                      Just NoCleanBefore -> ["--no-clean"]
                      Just NoCleanAfter -> ["--no-cleanup-after"]
                      Just NoCleanAll -> ["--no-clean", "--no-cleanup-after"]
                      Just MockShortCircuit -> ["--short-circuit", "install"]
          mockopts_common c = [c, "--root", mockRoot rootBr march] ++ noclean ++ ["--enable-network" | network]
          mockbuild_opts = mockopts_common command ++ ["--config-opts=cleanup_on_failure=False" | mnoclean `elem` [Nothing, Just NoCleanBefore]] ++ resultdir ++ srpms
          mockshell_opts = mockopts_common "--shell" ++ ["--no-clean" | "--no-clean" `notElem` noclean]
      if dryrun
        then do
        unless (mockmode == Just ShellOnly) $
          cmdN "mock" mockbuild_opts
        when (reqShell mockmode) $ cmdN "mock" mockshell_opts
        else do
        ok <-
          if mockmode == Just ShellOnly
          then return True
          else do
            dobuild <-
              if mockmode == Just MockInstall
              then do
                let verrel = showPkgVerRel . readNVRA
                anyM (\s -> not <$> doesFileExist ("results" </> verrel s </> s)) srpms
              else return True
            if dobuild
              then cmdBool "mock" mockbuild_opts
              else return True
        when (reqShell mockmode) $ cmd_ "mock" mockshell_opts
        unless ok $ error' "mockbuild failed"
        when (mockmode == Just MockInstall) $ do
          mapM_ mockInstall pkgs
      where
        prepSrpm :: AnyBranch -> FilePath -> IO FilePath
        prepSrpm rbr pkgdir =
          withExistingDirectory pkgdir $ do
            pkg <- getPackageName pkgdir
            putPkgHdr pkg
            whenM isPkgGitRepo $
              unless noswitch $
              gitSwitchBranch rbr
            spec <- findSpecfile
            generateSrpm Nothing spec

        mockInstall :: String -> IO ()
        mockInstall pkg = do
          spec <- findSpecfile
          rpms <- builtRpms (RelBranch br) spec
          let nvras = map readNVRA rpms
          -- FIXME can this be removed now?
          already <- filterM nvraInstalled nvras
          if null already || reinstall
            then doInstallRPMs rpms
            else putStrLn $ unlines (map showNVRA already) ++
                 "\nalready installed!\n"
            where
              doInstallRPMs rpms = do
                whenJust (headMay rpms) $ \rpm -> do
                  let nvra = readNVRA rpm
                  -- FIXME show source NVR (eg not pandoc-common)
                  putStrLn $ (showNVR . dropArch) nvra
                  let nvras = rpmsToNVRAs rpms
                  -- FIXME: prefix = fromMaybe (nvrName nvr) mprefix
                  decided <- -- decideRPMs yes False mexisting select pkg nvras
                    decideRPMs Yes False Nothing selectDefault pkg nvras
                  -- FIXME dryrun and debug
                  -- FIXME return Bool?
                  let verrel = showPkgVerRel nvra
                      results = "results" </> verrel
                  forM_ ["noarch", "x86_64"] $ \arch -> do
                    let link = results </> arch
                    unlessM (doesDirectoryExist link) $
                      createFileLink "." link
                  installRPMs False False Nothing Yes $ groupOnArch results decided
