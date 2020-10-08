module Main (main) where

import Distribution.Fedora.Branch
import Options.Applicative (eitherReader, ReadM)
import SimpleCmdArgs

-- commands
import Cmd.Bugs
import Cmd.Build
import Cmd.Bump
import Cmd.Clone
import Cmd.Commit
import Cmd.Copr
import Cmd.Diff
import Cmd.Import
import Cmd.Local
import Cmd.Log
import Cmd.Merge
import Cmd.Mock
import Cmd.Override
import Cmd.Parallel
import Cmd.PkgReview
import Cmd.Pull
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.Reviews
import Cmd.Scratch
import Cmd.SideTags
import Cmd.Status
import Cmd.Switch

import Branches
import Common.System
import Git (CommitOpt(..))
import ListReviews
import Package (ForceShort(..))
import Paths_fbrnch (version)

main :: IO ()
main = do
  setNoBuffering
  simpleCmdArgs (Just version) "Fedora package branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "clone" "clone packages" $
      cloneCmd <$> optional branchOpt <*> cloneRequest
    , Subcommand "switch" "Switch branch" $
      switchCmd <$> branchesPackages
    , Subcommand "nvr" "Print name-version-release" $
      nvrCmd <$> branchesOpt <*> branchesPackages
    , Subcommand "status" "Status package/branch status" $
      statusCmd <$> switchWith 'n' "no-fetch" "Do not git fetch to save time" <*> switchWith 'r' "reviews" "Status of reviewed packages" <*> branchesOpt <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd <$> branchesPackages
    , Subcommand "build" "Build package(s) in Koji" $
      buildCmd <$> buildOpts <*> branchesOpt <*> branchesPackages
    , Subcommand "parallel" "Parallel build packages in Koji" $
      parallelBuildCmd <$> dryrunOpt <*> sidetagTargetOpt <*> branchesOpt <*> branchesPackages
    , Subcommand "sidetags" "List user's side-tags" $
      sideTagsCmd <$> many branchArg
    , Subcommand "override" "Tag builds into buildroot override in Koji" $
      overrideCmd <$> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd <$> dryrunOpt <*> rebuildSrpmOpt <*> noFailFastOpt <*> optional archesOpt <*> mtargetOpt <*> branchesPackages
    , Subcommand "sort" "Sort packages in build dependency order" $
      sortCmd <$> optional rpmWithOpt <*> branchesPackages
    , Subcommand "prep" "Prep sources" $
      prepCmd <$> branchesPackages
    , Subcommand "local" "Build locally" $
      localCmd <$> optional forceshortOpt <*> branchesPackages
    , Subcommand "srpm" "Build srpm" $
      srpmCmd <$> branchesPackages
    , Subcommand "diff" "Diff local changes" $
      diffCmd <$> diffSpecOnly <*> diffWorkOpt <*> diffFormatOpt <*> diffBranchOpt <*> branchesPackages
    , Subcommand "log" "Show commits between branches" $
      logCmd <$> switchWith 'l' "long" "show full commit log" <*> anyBranchArg <*> anyBranchArg <*> many (pkgArg "PACKAGE...")
    , Subcommand "mock" "Local mock build" $
      mockCmd <$> switchWith 'n' "no-clean" "Do not clean chroot before building a package" <*> switchWith 'N' "no-clean-after" "Don't clean  chroot after building a package" <*> optional (optionWith branchM 'r' "root" "BRANCH" "Mock config to use") <*> branchesPackages
    , Subcommand "install-deps" "Install package build dependencies" $
      installDepsCmd <$> branchesPackages
    , Subcommand "install" "Build locally and install package(s)" $
      -- FIXME drop --shortcircuit from install?
      installCmd <$> optional forceshortOpt <*> switchWith 'r' "reinstall" "reinstall rpms" <*> branchesPackages
    , Subcommand "bugs" "List package bugs" $
      bugsCmd <$> many (pkgArg "PACKAGE...")
    , Subcommand "bump" "Bump release for package" $
      bumpPkgs <$> optional commitOpts <*> branchesOpt <*> branchesPackages
    , Subcommand "commit" "Git commit packages" $
      commitPkgs <$> optional commitOpts <*> many (pkgArg "PACKAGE...")
    , Subcommand "pull" "Git pull packages" $
      pullPkgs <$> branchesPackages
    , Subcommand "create-review" "Create a Package Review request" $
      createReview <$> noScratchBuild <*> mockOpt <*> many (pkgArg "PACKAGE...")
    , Subcommand "update-review" "Update a Package Review" $
      updateReview <$> noScratchBuild <*> mockOpt <*> optional (strArg "SPECFILE")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd <$> reviewShortOpt <*> reviewAllStatusOpt <*> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos <$> switchWith 'r' "retry" "Re-request repo" <*> many (pkgArg "NEWPACKAGE...")
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd <$> many (pkgArg "NEWPACKAGE...")
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranches <$> mockOpt <*> optional branchesRequestOpt <*> branchesPackages
    , Subcommand "find-review" "Find package review bug" $
      findReview <$> pkgArg "PACKAGE"
--    , Subcommand "test-bz-token" "Check bugzilla login status" $
--      pure testBZlogin
    , Subcommand "command" "Run shell command in package dirs ($p)" $
      commandCmd <$> commandOpt <*> branchesOpt <*> branchesPackages
    , Subcommand "copr" "Build package(s) in Fedora Copr" $
      coprCmd <$> dryrunOpt <*> buildByOpt <*> many archOpt <*> strArg "PROJECT" <*> branchesOpt <*> branchesPackages
    ]
  where
    cloneRequest :: Parser CloneRequest
    cloneRequest = flagWith' (CloneUser Nothing) 'M' "mine" "Your packages" <|> CloneUser . Just <$> strOptionWith 'u' "user" "USER" "Packages of FAS user" <|> ClonePkgs <$> some (pkgArg "PACKAGE...")

    noScratchBuild = switchWith 'S' "no-scratch-build" "Skip Koji scratch build"

    reviewShortOpt :: Parser Bool
    reviewShortOpt = switchWith 's' "short" "Only output the package name"

    reviewAllStatusOpt :: Parser Bool
    reviewAllStatusOpt = switchWith 'A' "all-status" "Output all open reviews"

    reviewStatusOpt :: Parser ReviewStatus
    reviewStatusOpt =
      flagWith' ReviewUnApproved 'u' "unapproved" "Package reviews not yet approved" <|>
      flagWith' ReviewApproved 'a' "approved" "All open approved package reviews" <|>
      flagWith' ReviewWithoutRepoReq 'w' "without-request" "Approved package reviews without a repo request" <|>
      flagWith' ReviewRepoRequested 'r' "requested" "Approved package reviews with a pending repo request" <|>
      flagWith' ReviewRepoCreated 'c' "created" "Approved package reviews with a created repo" <|>
      flagWith' ReviewUnbranched 'B' "unbranched" "Approved created package reviews not yet branched" <|>
      flagWith ReviewAllOpen ReviewBranched 'b' "branched" "Approved created package reviews already branched"

    branchOpt :: Parser Branch
    branchOpt = optionWith branchM 'b' "branch" "BRANCH" "branch"

    branchArg :: Parser Branch
    branchArg = argumentWith branchM "BRANCH"

    anyBranchArg :: Parser AnyBranch
    anyBranchArg = argumentWith anyBranchM "BRANCH"

    branchM :: ReadM Branch
    branchM = eitherReader eitherBranch'

    anyBranchM :: ReadM AnyBranch
    anyBranchM = anyBranch <$> str

    pkgArg :: String -> Parser String
    pkgArg lbl = removeSuffix "/" <$> strArg lbl

    branchesOpt :: Parser (Maybe BranchOpts)
    branchesOpt =
      optional (flagWith' AllBranches 'B' "all-branches" "All active release branches" <|>
                ExcludeBranches <$> some excludeBranchOpt)

    excludeBranchOpt :: Parser Branch
    excludeBranchOpt = optionWith branchM 'x' "exclude-branch" "BRANCH" "branch"

    branchesPackages :: Parser [String]
    branchesPackages = many (pkgArg "[BRANCH]... [PACKAGE]...")

    branchesRequestOpt :: Parser BranchOpts
    branchesRequestOpt =
      flagWith' AllBranches 'B' "all-branches" "Request branches for all current releases [default latest 2]" <|>
      ExcludeBranches <$> some excludeBranchOpt

    rpmWithOpt :: Parser RpmWith
    rpmWithOpt =
      RpmWith <$> strOptionWith 'w' "with" "BCOND" "rpmspec --with option" <|>
      RpmWithout <$> strOptionWith 'W' "without" "BCOND" "rpmspec --without option"

    mockOpt = switchWith 'm' "mock" "Do mock build to test"

    archOpt :: Parser String
    archOpt = strOptionWith 'a' "arch" "ARCH[,ARCH].." "build for arch(s)"

    rebuildSrpmOpt = switchWith 's' "rebuild-srpm" "rebuild srpm in Koji"

    buildOpts = BuildOpts <$> mergeOpt <*> noFailFastOpt <*> mtargetOpt <*> overrideOpt <*> dryrunOpt <*> updatetypeOpt

--    yesOpt = switchWith 'y' "yes" "Assume yes for questions"

    mergeOpt = switchWith 'm' "merge" "merge from newer branch"

    noFailFastOpt = switchWith 'f' "no-fast-fail" "Do not --fast-fail"

    excludeArch :: Parser String
    excludeArch = strOptionWith 'X' "exclude-arch" "ARCH[,ARCH].." "build without arch(s)"

    archesOpt :: Parser Archs
    archesOpt = Archs <$> some archOpt <|> ExcludedArchs <$> some excludeArch

    mtargetOpt :: Parser (Maybe String)
    mtargetOpt = optional targetOpt

    targetOpt :: Parser String
    targetOpt = strOptionWith 't' "target" "TARGET" "Koji target"

    overrideOpt = switchWith 'o' "override" "Create a buildroot override and wait-repo"

    dryrunOpt = switchWith 'n' "dry-run" "Do not write (push, build, post, override)"

    updatetypeOpt = flagWith' Nothing 'U' "no-update" "Do not generate a Bodhi update" <|> Just <$> optionalWith auto 'u' "update-type" "TYPE" "security, bugfix, enhancement (default), or newpackage" EnhancementUpdate

    forceshortOpt =
      flagWith' ForceBuild 'f' "rebuild" "Rebuild even if already built" <|>
      flagWith' ShortCircuit 's' "short-circuit" "Do --short-circuit rpmbuild"

    diffFormatOpt :: Parser DiffFormat
    diffFormatOpt =
      DiffContext <$> optionWith auto 'u' "unified" "CONTEXT" "Lines of context" <|>
      flagWith' DiffMinimal 'm' "minimal" "Minimize diff noise" <|>
      flagWith' DiffStats 's' "stats" "Minimize diff noise" <|>
      flagWith DiffDefault DiffQuiet 'q' "quiet" "Just output package name"

    diffSpecOnly :: Parser Bool
    diffSpecOnly = switchWith 'o' "spec-only" "Only diff spec file"

    diffWorkOpt :: Parser DiffWork
    diffWorkOpt =
      flagWith' DiffWorkStaged 'S' "staged" "Diff staged changes (git diff --cached)" <|>
      flagWith DiffWorkAll DiffWorkUnstage 'U' "unstaged" "Diff unstaged changes (git diff) [default is 'git diff HEAD']"

    diffBranchOpt :: Parser (Maybe AnyBranch)
    diffBranchOpt = optional (optionWith anyBranchM 'w' "with-branch" "BRANCH" "branch")

    commitOpts :: Parser CommitOpt
    commitOpts =
      CommitMsg <$> strOptionWith 'm' "message" "COMMITMSG" "commit message" <|>
      flagWith' CommitAmend 'a' "amend" "Amend commit"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

    commandOpt = strOptionWith 'c' "cmd" "COMMAND" "Shell command to run in $p"

    sidetagTargetOpt :: Parser (Maybe SideTagTarget)
    sidetagTargetOpt =
      optional (Target <$> targetOpt <|>
                flagWith' SideTag 's' "side-tag" "Use existing branch side-tag for building: create 'fedpkg request-side-tag --base-tag'")
