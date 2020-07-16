module Main (main) where

import Common

import Distribution.Fedora.Branch
import Options.Applicative (eitherReader, ReadM)
import SimpleCmd
import SimpleCmd.Git
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

-- commands
import Cmd.Bugs
import Cmd.Build
import Cmd.Clone
import Cmd.Import
import Cmd.Local
import Cmd.Merge
import Cmd.PkgReview
import Cmd.Pull
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.Reviews
import Cmd.Status
import Cmd.Switch

import Bugzilla (testBZlogin)
import ListReviews
import Paths_fbrnch (version)

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  -- FIXME? some commands do not use branches
  activeBranches <- getFedoraBranches
  gitdir <- isGitDir "."
  dispatchCmd gitdir activeBranches

dispatchCmd :: Bool -> [Branch] -> IO ()
dispatchCmd gitdir activeBranches =
  simpleCmdArgs (Just version) "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "clone" "clone packages" $
      cloneCmd <$> optional branchOpt <*> cloneRequest
    , Subcommand "switch" "Switch branch" $
      switchCmd <$> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
    , Subcommand "status" "Status package/branch status" $
      statusCmd <$> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd <$> branchesPackages
    , Subcommand "build" "Build package(s)" $
      buildCmd <$> buildOpts <*> branchesPackages
    , Subcommand "parallel" "Parallel build packages in Koji" $
      parallelBuildCmd <$> targetOpt <*> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd <$> rebuildSrpmOpt <*> noFailFastOpt <*> optional archOpt <*> targetOpt <*> localBranchPackages
    , Subcommand "sort" "Sort packages in build dependency order" $
      sortCmd <$> localBranchPackages
    , Subcommand "prep" "Prep sources" $
      prepCmd <$> localBranchPackages
    , Subcommand "local" "Build locally" $
      localCmd <$> shortcircuitOpt <*> localBranchPackages
    , Subcommand "srpm" "Build srpm" $
      srpmCmd <$> localBranchPackages
    , Subcommand "diff" "Diff local changes" $
      diffCmd <$> diffWorkOpt <*> diffFormatOpt <*> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
    , Subcommand "mock" "Local mock build" $
      mockCmd <$> localBranchPackages
    , Subcommand "install" "Build locally and install package(s)" $
      installCmd <$> switchWith 'r' "reinstall" "use dnf reinstall" <*> localBranchPackages
    , Subcommand "bugs" "List package bugs" $
      bugsCmd <$> optional (pkgArg "PACKAGE")
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> some (pkgArg "PACKAGE...")
    , Subcommand "create-review" "Create a Package Review request" $
      createReview <$> noScratchBuild <*> mockOpt <*> optional (strArg "SPECFILE")
    , Subcommand "update-review" "Update a Package Review" $
      updateReview <$> noScratchBuild <*> mockOpt <*> optional (strArg "SPECFILE")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd <$> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos <$> switchWith 'r' "retry" "Re-request repo" <*> many (pkgArg "NEWPACKAGE...")
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd <$> many (pkgArg "NEWPACKAGE...")
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranches <$> mockOpt <*> branchesRequestOpt <*> many pkgOpt
    , Subcommand "find-review" "Find package review bug" $
      review <$> pkgArg "PACKAGE"
    , Subcommand "test-bz-token" "Check bugzilla login status" $
      pure testBZlogin
    ]
  where
    cloneRequest :: Parser CloneRequest
    cloneRequest = flagWith' (CloneUser Nothing) 'M' "mine" "Your packages" <|> CloneUser . Just <$> strOptionWith 'u' "user" "USER" "Packages of FAS user" <|> ClonePkgs <$> some (pkgArg "PACKAGE...")

    noScratchBuild = switchWith 'S' "no-scratch-build" "Skip Koji scratch build"

    reviewStatusOpt :: Parser ReviewStatus
    reviewStatusOpt =
      flagWith' ReviewUnApproved 'u' "unapproved" "Package reviews not yet approved" <|>
      flagWith' ReviewApproved 'a' "approved" "All open approved package reviews" <|>
      flagWith' ReviewWithoutRepoReq 'w' "without-request" "Approved package reviews without a repo request" <|>
      flagWith' ReviewRepoRequested 'r' "requested" "Approved package reviews with a pending repo request" <|>
      flagWith' ReviewRepoCreated 'c' "created" "Approved package reviews with a created repo" <|>
      flagWith ReviewAllOpen ReviewUnbranched 'b' "unbranched" "Approved created package reviews not yet branched"

    branchOpt :: Parser Branch
    branchOpt = optionWith branchM 'b' "branch" "BRANCH" "branch"

    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH.."

    branchM :: ReadM Branch
    branchM = eitherReader (eitherActiveBranch activeBranches)

    anyBranchOpt :: Parser Branch
    anyBranchOpt = optionWith anyBranchM 'b' "branch" "BRANCH" "branch"

    anyBranchArg :: Parser Branch
    anyBranchArg = argumentWith anyBranchM "BRANCH.."

    anyBranchM :: ReadM Branch
    anyBranchM = eitherReader eitherBranch

    pkgArg :: String -> Parser String
    pkgArg lbl = removeSuffix "/" <$> strArg lbl

    pkgOpt :: Parser String
    pkgOpt = removeSuffix "/" <$> strOptionWith 'p' "package" "PKG" "package"

    branchesPackages :: Parser ([Branch],[String])
    branchesPackages = if gitdir then
      pairSort <$> (many branchOpt <|> many branchArg) <*> pure []
      else pairSort <$> many branchOpt <*> many (pkgArg "PACKAGE..") <|>
           pairSort <$> many branchArg <*> some pkgOpt

    pairSort a b = ((reverse . sort) a, b)

    localBranchPackages :: Parser (Maybe Branch,[String])
    localBranchPackages = if gitdir
      then (,) <$> optional (branchOpt <|> branchArg) <*> pure []
      else (,) <$> optional branchOpt <*> many (pkgArg "PACKAGE..") <|>
           (,) <$> optional branchArg <*> some pkgOpt

    branchesRequestOpt :: Parser BranchesRequest
    branchesRequestOpt = flagWith' AllReleases 'a' "all" "Request branches for all current releases [default latest 2]" <|> BranchesRequest <$> many branchArg

    mockOpt = switchWith 'm' "mock" "Do mock build to test"

    archOpt :: Parser String
    archOpt = strOptionWith 'a' "arch" "ARCH[,ARCH].." "Scratch build for arch(s)"

    rebuildSrpmOpt = switchWith 's' "rebuild-srpm" "rebuild srpm in Koji"

    buildOpts = BuildOpts <$> mergeOpt <*> noFailFastOpt <*> targetOpt <*> overrideOpt

    mergeOpt = switchWith 'm' "merge" "merge from newer branch"

    noFailFastOpt = switchWith 'f' "no-fast-fail" "Do not --fast-fail"

    targetOpt :: Parser (Maybe String)
    targetOpt = optional (strOptionWith 't' "target" "TARGET" "Koji target")

    overrideOpt = switchWith 'o' "override" "Create a buildroot override and wait-repo"

    shortcircuitOpt = switchWith 's' "short-circuit" "Do --short-circuit rpmbuild"

    diffFormatOpt :: Parser DiffFormat
    diffFormatOpt =
      flagWith DiffDefault DiffShort 's' "short" "Just output package name" <|>
      DiffContext <$> optionWith auto 'u' "unified" "CONTEXT" "Lines of context"

    diffWorkOpt :: Parser DiffWork
    diffWorkOpt =
      flagWith' DiffWorkStaged 'S' "staged" "Diff staged changes (git diff --cached)" <|>
      flagWith DiffWorkAll DiffWorkUnstage 'U' "unstaged" "Diff unstaged changes (git diff) [default is 'git diff HEAD']"
