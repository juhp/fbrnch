module Main (main) where

import Common

import Distribution.Fedora.Branch
import Options.Applicative (eitherReader, ReadM)
import SimpleCmd
import SimpleCmdArgs
import System.IO (BufferMode(NoBuffering), hSetBuffering, hIsTerminalDevice, stdin, stdout)

-- commands
import Cmd.Bugs
import Cmd.Build
import Cmd.Clone
import Cmd.Commit
import Cmd.Copr
import Cmd.Diff
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

import Branches
import ListReviews
import Package (ForceShort(..))
import Paths_fbrnch (version)

main :: IO ()
main = do
  tty <- hIsTerminalDevice stdin
  when tty $ hSetBuffering stdout NoBuffering
  -- FIXME? some commands do not use branches
  simpleCmdArgs (Just version) "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "clone" "clone packages" $
      cloneCmd <$> optional branchOpt <*> cloneRequest
    , Subcommand "switch" "Switch branch" $
      switchCmd <$> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
    , Subcommand "nvr" "Print name-version-release" $
      nvrCmd <$> branchesPackages
    , Subcommand "status" "Status package/branch status" $
      statusCmd <$> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd <$> branchesPackages
    , Subcommand "build" "Build package(s) in Koji" $
      buildCmd <$> buildOpts <*> branchesPackages
    , Subcommand "parallel" "Parallel build packages in Koji" $
      parallelBuildCmd <$> dryrunOpt <*> targetOpt <*> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd <$> rebuildSrpmOpt <*> noFailFastOpt <*> many archOpt <*> targetOpt <*> localBranchPackages
    , Subcommand "sort" "Sort packages in build dependency order" $
      sortCmd <$> optional rpmWithOpt <*> localBranchPackages
    , Subcommand "prep" "Prep sources" $
      prepCmd <$> localBranchPackages
    , Subcommand "local" "Build locally" $
      localCmd <$> optional forceshortOpt <*> localBranchPackages
    , Subcommand "srpm" "Build srpm" $
      srpmCmd <$> localBranchPackages
    , Subcommand "diff" "Diff local changes" $
      diffCmd <$> diffWorkOpt <*> diffFormatOpt <*> (anyBranchOpt <|> anyBranchArg) <*> many (pkgArg "PACKAGE...")
    , Subcommand "mock" "Local mock build" $
      mockCmd <$> optional (optionWith branchM 'r' "root" "BRANCH" "Mock config to use") <*> localBranchPackages
    , Subcommand "install-deps" "Install package build dependencies" $
      installDepsCmd <$> localBranchPackages
    , Subcommand "install" "Build locally and install package(s)" $
      -- FIXME drop --shortcircuit from install?
      installCmd <$> optional forceshortOpt <*> switchWith 'r' "reinstall" "reinstall rpms" <*> localBranchPackages
    , Subcommand "bugs" "List package bugs" $
      bugsCmd <$> optional (pkgArg "PACKAGE")
    , Subcommand "commit" "Git commit packages" $
      commitPkgs <$> commitOpts <*> some (pkgArg "PACKAGE...")
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> localBranchPackages
    , Subcommand "create-review" "Create a Package Review request" $
      createReview <$> noScratchBuild <*> mockOpt <*> many (pkgArg "PACKAGE...")
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
      findReview <$> pkgArg "PACKAGE"
--    , Subcommand "test-bz-token" "Check bugzilla login status" $
--      pure testBZlogin
    , Subcommand "copr" "Build package(s) in Fedora Copr" $
      coprCmd <$> dryrunOpt <*> buildByOpt <*> many archOpt <*> strArg "PROJECT" <*> branchesPackages
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
    branchM = eitherReader eitherBranch'

    anyBranchOpt :: Parser AnyBranch
    anyBranchOpt = optionWith anyBranchM 'b' "branch" "BRANCH" "branch"

    anyBranchArg :: Parser AnyBranch
    anyBranchArg = argumentWith anyBranchM "BRANCH.."

    anyBranchM :: ReadM AnyBranch
    anyBranchM = anyBranch <$> str

    pkgArg :: String -> Parser String
    pkgArg lbl = removeSuffix "/" <$> strArg lbl

    pkgOpt :: Parser String
    pkgOpt = removeSuffix "/" <$> strOptionWith 'p' "package" "PKG" "package"

    branchesOpt :: Parser Branches
    branchesOpt =
      -- FIXME was: reverse . sort
      BranchList <$> many anyBranchOpt <|>
      flagWith' AllBranches 'B' "all-branches" "All active release branches" <|>
      ExcludeBranches <$> many excludeBranchOpt

    excludeBranchOpt :: Parser Branch
    excludeBranchOpt = optionWith branchM 'x' "exclude-branch" "BRANCH" "branch"

    branchesPackages :: Parser (Branches,[String])
    branchesPackages =
      (,) <$> branchesOpt <*> many (pkgArg "PACKAGE...")

    localBranchPackages :: Parser (Maybe Branch,[String])
    localBranchPackages =
      (,) <$> optional branchOpt <*> many (pkgArg "PACKAGE...") <|>
      (,) <$> optional branchArg <*> some pkgOpt

    branchesRequestOpt :: Parser Branches
    branchesRequestOpt =
      flagWith' AllBranches 'B' "all-branches" "Request branches for all current releases [default latest 2]" <|>
      BranchList . fmap RelBranch <$> many branchArg <|>
      ExcludeBranches <$> many branchArg

    rpmWithOpt :: Parser RpmWith
    rpmWithOpt =
      RpmWith <$> strOptionWith 'w' "with" "BCOND" "rpmspec --with option" <|>
      RpmWithout <$> strOptionWith 'W' "without" "BCOND" "rpmspec --without option"

    mockOpt = switchWith 'm' "mock" "Do mock build to test"

    archOpt :: Parser String
    archOpt = strOptionWith 'a' "arch" "ARCH[,ARCH].." "Scratch build for arch(s)"

    rebuildSrpmOpt = switchWith 's' "rebuild-srpm" "rebuild srpm in Koji"

    buildOpts = BuildOpts <$> mergeOpt <*> noFailFastOpt <*> targetOpt <*> overrideOpt <*> dryrunOpt <*> updatetypeOpt

--    yesOpt = switchWith 'y' "yes" "Assume yes for questions"

    mergeOpt = switchWith 'm' "merge" "merge from newer branch"

    noFailFastOpt = switchWith 'f' "no-fast-fail" "Do not --fast-fail"

    targetOpt :: Parser (Maybe String)
    targetOpt = optional (strOptionWith 't' "target" "TARGET" "Koji target")

    overrideOpt = switchWith 'o' "override" "Create a buildroot override and wait-repo"

    dryrunOpt = switchWith 'n' "dry-run" "Do not write (push, build, post, override)"

    updatetypeOpt = optionalWith auto 'u' "update-type" "TYPE" "security, bugfix, enhancement (default), or newpackage" EnhancementUpdate

    forceshortOpt =
      flagWith' ForceBuild 'f' "rebuild" "Rebuild even if already built" <|>
      flagWith' ShortCircuit 's' "short-circuit" "Do --short-circuit rpmbuild"

    diffFormatOpt :: Parser DiffFormat
    diffFormatOpt =
      DiffContext <$> optionWith auto 'u' "unified" "CONTEXT" "Lines of context" <|>
      flagWith' DiffMinimal 'm' "minimal" "Minimize diff noise" <|>
      flagWith' DiffStats 's' "stats" "Minimize diff noise" <|>
      flagWith DiffDefault DiffQuiet 'q' "quiet" "Just output package name"

    diffWorkOpt :: Parser DiffWork
    diffWorkOpt =
      flagWith' DiffWorkStaged 'S' "staged" "Diff staged changes (git diff --cached)" <|>
      flagWith DiffWorkAll DiffWorkUnstage 'U' "unstaged" "Diff unstaged changes (git diff) [default is 'git diff HEAD']"

    commitOpts :: Parser CommitOpt
    commitOpts =
      CommitMsg <$> strOptionWith 'm' "message" "COMMITMSG" "commit message" <|>
      flagWith' CommitAmend 'a' "amend" "Amend commit"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"
