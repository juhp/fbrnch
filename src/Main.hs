{-# LANGUAGE CPP #-}

module Main (main) where

import Distribution.Fedora.Branch
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (maybeReader, ReadM)
#endif
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
import Cmd.Install
import Cmd.ListBranches
import Cmd.ListPackages
import Cmd.Local
import Cmd.Log
import Cmd.Merge
import Cmd.Mock
import Cmd.Override
import Cmd.Parallel
import Cmd.PkgReview
import Cmd.Prep
import Cmd.Pull
import Cmd.Repoquery
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.Reviews
import Cmd.Scratch
import Cmd.SideTags
import Cmd.Sort
import Cmd.Status
import Cmd.Switch
import Cmd.Update
import Cmd.WaitRepo

import Branches
import Common.System
import Git (CommitOpt(..))
import ListReviews
import Package (ForceShort(..), BCond(..))
import Paths_fbrnch (version)

main :: IO ()
main = do
  setNoBuffering
  simpleCmdArgs (Just version) "Fedora branch building tool"
    "This tool helps with updating and building package branches" $
    subcommands
    [ Subcommand "clone" "clone packages" $
      cloneCmd
      <$> optional branchOpt
      <*> cloneRequest
    , Subcommand "switch" "Switch branch" $
      switchCmd
      <$> anyBranchArg
      <*> manyPackages
    , Subcommand "nvr" "Print name-version-release" $
      nvrCmd
      <$> branchesPackages
    , Subcommand "status" "Status package/branch status" $
      statusCmd
      <$> switchWith 'n' "no-fetch" "Do not git fetch to save time"
      <*> switchWith 'r' "reviews" "Status of reviewed packages"
      <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd
      <$> nopromptOpt
      <*> branchesPackages
    , Subcommand "build" "Build package(s) in Koji" $
      buildCmd
      <$> buildOpts
      <*> branchesPackages
    , Subcommand "list" "List packages in pagure" $
      listCmd
      <$> switchWith 'c' "count" "Print number of packages"
      <*> optional packagerOpt
      <*> many (pkgArg "PKGPAT...")
    , Subcommand "branches" "List package branches" $
      branchesCmd
      <$> switchWith 'd' "skip-dead" "Skip if dead.package exists"
      <*> switchWith 'a' "all" "List all branches"
      <*> switchWith 'm' "missing" "Show missing branches"
      <*> branchesModeOpt
      <*> branchesPackages
    , Subcommand "parallel" "Parallel build packages in Koji" $
      parallelBuildCmd
      <$> dryrunOpt
      <*> optionalWith auto 'l' "skip-to-layer" "LAYERNO" "Skip the first N layers [default 0]" 0
      <*> optional sidetagTargetOpt
      <*> updatetypeOpt
      <*> branchesPackages
    , Subcommand "sidetags" "List user's side-tags" $
      sideTagsCmd
      <$> many branchArg
    , Subcommand "override" "Tag builds into buildroot override in Koji" $
      overrideCmd
      <$> dryrunOpt
      <*> optional (optionWith auto 'd' "duration" "DAYS" "Number of days until expiry [default 4]")
      <*> switchWith 'w' "no-wait" "Skip waitrepo step"
      <*> branchesPackages
    , Subcommand "waitrepo" "Wait for build to appear in Koji buildroot" $
      waitrepoCmd
      <$> dryrunOpt
      <*> mtargetOpt
      <*> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd
      <$> dryrunOpt
      <*> rebuildSrpmOpt
      <*> noFailFastOpt
      <*> optional archesOpt
      <*> mtargetOpt
      <*> optional (strOptionWith 'r' "ref" "COMMITHASH" "git commit to build")
      <*> branchesPackages
    , Subcommand "update" "Update package in dist-git to newer version" $
      updateCmd
      <$> switchWith 's' "sources-only" "Only update sources"
      <*> switchWith 'H' "allow-head" "For updating inside rebase"
      <*> maybeBranchPackages False
    , Subcommand "sort" "Sort packages in build dependency order" $
      sortCmd
      <$> optional rpmWithOpt
      <*> maybeBranchPackages True
    , Subcommand "prep" "Prep sources" $
      prepCmd
      <$> switchWith 'c' "clone" "Try to clone package repo first"
      <*> maybeBranchPackages False
    , Subcommand "local" "Build locally" $
      localCmd
      <$> optional forceshortOpt
      <*> many bcondOpt
      <*> branchesPackages
    , Subcommand "srpm" "Build srpm" $
      srpmCmd
      <$> switchWith 'f' "force" "regenerate even if spec older than existing srpm"
      <*> maybeBranchPackages False
    , Subcommand "diff" "Diff local changes" $
      diffCmd
      <$> switchWith 'o' "spec-only" "Only diff spec file"
      <*> diffWorkOpt
      <*> diffFormatOpt
      <*> switchWith 'q' "quiet" "Just output package name"
      <*> optional diffFilterOpt
      <*> optional (optionWith anyBranchM 'w' "with-branch" "BRANCH" "branch")
      <*> maybeBranchPackages False
    , Subcommand "compare" "Show commits between branches" $
      logCmd
      <$> switchWith 'l' "long" "show full commit log"
      <*> anyBranchArg
      <*> anyBranchArg
      <*> manyPackages
    , Subcommand "mock" "Local mock build" $
      mockCmd
      <$> switchWith 'd' "dry-run" "Do not build (but creates srpm)"
      <*> switchWith 'n' "no-clean" "Do not clean chroot before building a package"
      <*> switchWith 'w' "network" "Use network during build"
      <*> switchWith 'N' "no-clean-after" "Don't clean  chroot after building a package"
      <*> optional (optionWith branchM 'r' "root" "BRANCH" "Mock config to use")
      <*> branchesPackages
    , Subcommand "install-deps" "Install package build dependencies" $
      installDepsCmd
      <$> maybeBranchPackages False
    , Subcommand "install" "Build locally and install package(s)" $
      -- FIXME drop --shortcircuit from install?
      installCmd
      <$> switchWith 'v' "verbose" "verbose rpmbuild output"
      <*> switchWith 'R' "recurse" "build and install missing deps packages"
      <*> optional forceshortOpt
      <*> many bcondOpt
      <*> switchWith 'r' "reinstall" "reinstall rpms"
      <*> maybeBranchPackages False
    , Subcommand "not-installed" "Packages not installed locally" $
      notInstalledCmd
      <$> maybeBranchPackages False
    , Subcommand "bugs" "List package bugs" $
      bugsCmd
      <$> optional (strOptionWith 's' "summary" "KEY" "Search for bugs containing keyword")
      <*> manyPackages
    , Subcommand "bump" "Bump release for package" $
      bumpPkgs
      <$> switchWith 'l' "local" "Use local origin rather than checking latest koji"
      <*> optional commitOpts
      <*> branchesPackages
    , Subcommand "commit" "Git commit packages" $
      commitPkgs
      <$> optional commitOpts
      <*> manyPackages
    , Subcommand "pull" "Git pull packages" $
      pullPkgs
      <$> branchesPackages
    , Subcommand "create-review" "Create a Package Review request" $
      createReview
      <$> scratchOpt
      <*> mockOpt False
      <*> manyPackages
    , Subcommand "update-review" "Update a Package Review" $
      updateReview
      <$> scratchOpt
      <*> mockOpt False
      <*> optional (strArg "SPECFILE")
    , Subcommand "review-package" "Run fedora-review on a package Review Request bug" $
      reviewPackage
      <$> optional (pkgArg "PACKAGE/BZID")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd
      <$> reviewShortOpt
      <*> reviewAllStatusOpt
      <*> switchWith 'T' "assigned-to" "List reviews assigned to user"
      <*> optional (strOptionWith 'U' "user" "USER" "Bugzilla user email")
      <*> optional (strOptionWith 'p' "pattern" "PKGPREFIX" "Package pattern prefix")
      <*> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos
      <$> mockOpt True
      <*> reviewAllStatusOpt
      <*> switchWith 'r' "retry" "Re-request repo"
      <*> branchesPackages
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd
      <$> mockOpt True
      <*> branchesPackages
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranches
      <$> mockOpt False
      <*> branchesPackages
    , Subcommand "find-review" "Find package review bug" $
      findReview
      <$> pkgArg "PACKAGE"
--    , Subcommand "test-bz-token" "Check bugzilla login status" $
--      pure testBZlogin
    , Subcommand "command" "Run shell command in package dirs ($p)" $
      commandCmd
      <$> switchWith 'o' "if-output" "only print if output"
      <*> switchWith '1' "compact" "print package on same line as output"
      <*> switchWith 'k' "continue" "keep going after an error"
      <*> commandOpt
      <*> branchesPackages
    , Subcommand "copr" "Build package(s) in Fedora Copr" $
      coprCmd
      <$> dryrunOpt
      <*> switchWith 'l' "list-chroots" "Show project chroots"
      <*> buildByOpt
      <*> many archOpt
      <*> pkgArg "PROJECT"
      <*> branchesPackages
    , Subcommand "rename-master" "Rename local master branch to rawhide" $
      renameMasterCmd
      <$> manyPackages
    , Subcommand "count" "Count number of living packages" $
      countCmd
      <$> maybeBranchPackages True
    , Subcommand "graph" "Output dependency graph" $
      graphCmd
      <$> switchWith 'o' "output" "Output graph in gv/dot format"
      <*> optional rpmWithOpt <*> maybeBranchPackages True
    , Subcommand "repoquery" "Repoquery branches (put repoquery options after '--')" $
      repoqueryCmd
      <$> branchesPackages
    ]
  where
    cloneRequest :: Parser CloneRequest
    cloneRequest = flagWith' (CloneUser Nothing) 'M' "mine" "Your packages" <|> CloneUser . Just <$> strOptionWith 'u' "user" "USER" "Packages of FAS user" <|> ClonePkgs <$> somePackages

    reviewShortOpt :: Parser Bool
    reviewShortOpt = switchWith 's' "short" "Only output the package name"

    reviewAllStatusOpt :: Parser Bool
    reviewAllStatusOpt = switchWith 'A' "all-status" "all open reviews"

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

    -- anyBranchOpt :: Parser AnyBranch
    -- anyBranchOpt = optionWith anyBranchM 'b' "branch" "BRANCH" "branch"

    branchM :: ReadM Branch
    branchM = maybeReader readBranch

    anyBranchM :: ReadM AnyBranch
    anyBranchM = anyBranch <$> str

    pkgArg :: String -> Parser String
    pkgArg lbl = removeSuffix "/" <$> strArg lbl

    manyPackages :: Parser [String]
    manyPackages =  many (pkgArg "PACKAGE...")

    somePackages :: Parser [String]
    somePackages = some (pkgArg "PACKAGE...")

    branchesOpt :: Parser (Maybe BranchOpts)
    branchesOpt =
      optional (flagWith' AllBranches 'B' "all-branches" "All active release branches" <|>
                flagWith' AllFedora 'F' "all-fedora" "All active Fedora branches" <|>
                flagWith' AllEPEL 'E' "all-epel" "All active EPEL branches" <|>
                ExcludeBranches <$> some excludeBranchOpt)

    excludeBranchOpt :: Parser Branch
    excludeBranchOpt = optionWith branchM 'x' "exclude-branch" "BRANCH" "branch"

    -- branchesRequestOpt :: Parser BranchOpts
    -- branchesRequestOpt =
    --   flagWith' AllBranches 'B' "all-branches" "Request branches for all current releases [default latest 2]" <|>
    --   ExcludeBranches <$> some excludeBranchOpt

    maybeBranchPackages :: Bool -> Parser (Maybe Branch,[String])
    maybeBranchPackages oneplus =
      maybeBranchesPkgs <$>
      if oneplus
      then some (pkgArg "[BRANCH] PACKAGE...")
      else many (pkgArg "[BRANCH] [PACKAGE]...")
      where
        maybeBranchesPkgs :: [String] -> (Maybe Branch,[String])
        maybeBranchesPkgs args =
          let (brs,pkgs) = partitionBranches args
          in case brs of
            [] -> (Nothing, pkgs)
            [br] -> (Just br,pkgs)
            _ -> error' $ "cannot have more than one branch: " ++ unwords (map show brs)

    branchesPackages :: Parser (BranchesReq, [String])
    branchesPackages =
      branchesReqPkgs <$> branchesOpt <*> many (pkgArg "BRANCH... PACKAGE...")
      where
        branchesReqPkgs :: Maybe BranchOpts -> [String] -> (BranchesReq, [String])
        branchesReqPkgs mbrnchopts args =
          let (brs,pkgs) = partitionBranches args
          in case mbrnchopts of
            Nothing -> (Branches brs, pkgs)
            Just req | null brs -> (BranchOpt req, pkgs)
                     | otherwise -> error' "cannot have branch option and branch list"

    rpmWithOpt :: Parser RpmWith
    rpmWithOpt =
      RpmWith <$> strOptionWith 'w' "with" "BCOND" "rpmspec --with option" <|>
      RpmWithout <$> strOptionWith 'W' "without" "BCOND" "rpmspec --without option"

    mockOpt brs = switchWith 'm' "mock" $ "Do mock build to test" ++ if brs then " branches" else ""

    archOpt :: Parser String
    archOpt = strOptionWith 'a' "arch" "ARCH[,ARCH].." "build for arch(s)"

    rebuildSrpmOpt = switchWith 's' "rebuild-srpm" "rebuild srpm in Koji"

    buildOpts = BuildOpts <$> mergeOpt <*> noFailFastOpt <*> mtargetOpt <*> overrideOpt <*> dryrunOpt <*> updatetypeOpt <*> useChangelogOpt

    useChangelogOpt = switchWith 'c' "changelog-notes" "Use spec changelog for Bodhi notes"

--    yesOpt = switchWith 'y' "yes" "Assume yes for questions"

    nopromptOpt = switchWith 'm' "no-prompt" "Merge without prompt"

    mergeOpt = optional (flagWith' True 'm' "merge" "Merge without prompt" <|>
                         flagWith' False 'M' "no-merge" "No merging")

    noFailFastOpt = switchWith 'f' "no-fail-fast" "Do not --fail-fast"

    excludeArch :: Parser String
    excludeArch = strOptionWith 'X' "exclude-arch" "ARCH[,ARCH].." "build without arch(s)"

    archesOpt :: Parser Archs
    archesOpt = Archs <$> some archOpt <|> ExcludedArchs <$> some excludeArch

    mtargetOpt :: Parser (Maybe String)
    mtargetOpt = optional targetOpt

    targetOpt :: Parser String
    targetOpt =
      checkNotRawhide <$> strOptionWith 't' "target" "TARGET" "Koji target"
      where
        checkNotRawhide "rawhide" = error' "'rawhide' is not a valid target!"
        checkNotRawhide t = t

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
      flagWith' DiffStatus 'n' "status" "Show diff --name-status" <|>
      flagWith DiffDefault DiffStats 's' "stats" "Show diff --stat"

    diffFilterOpt :: Parser DiffFilter
    diffFilterOpt =
      DiffMatch <$> strOptionWith 'f' "filter" "PATTERN" "filter diff with pattern" <|> DiffNotMatch <$> strOptionWith 'F' "filter-not" "PATTERN" "negative filter for diff with pattern"

    diffWorkOpt :: Parser DiffWork
    diffWorkOpt =
      flagWith' DiffWorkStaged 'S' "staged" "Diff staged changes (git diff --cached)" <|>
      flagWith DiffWorkAll DiffWorkUnstage 'U' "unstaged" "Diff unstaged changes (git diff) [default is 'git diff HEAD']"

    commitOpts :: Parser CommitOpt
    commitOpts =
      CommitMsg <$> strOptionWith 'm' "message" "COMMITMSG" "commit message" <|>
      flagWith' CommitAmend 'a' "amend" "Amend commit"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

    commandOpt = strOptionWith 'c' "cmd" "SHELLCOMMAND" "Shell command to run in $p"

    sidetagTargetOpt :: Parser SideTagTarget
    sidetagTargetOpt =
      Target <$> targetOpt <|>
      flagWith' SideTag 's' "sidetag" "Use the existing branch side-tag to build or creates one for you (with 'fedpkg request-side-tag --base-tag')"

    packagerOpt = Owner <$> strOptionWith 'o' "owner" "OWNER" "Package owner" <|> Committer <$> strOptionWith 'u' "username" "USERNAME" "Packages user can commit to"

    bcondOpt = BuildWith <$> strOptionWith 'w' "with" "FEATURE" "Turn on package FEATURE for build" <|>
               BuildWithout <$> strOptionWith 'W' "without" "FEATURE" "Turn off package FEATURE for build"

    branchesModeOpt :: Parser BranchesMode
    branchesModeOpt =
      flagWith' Remote 'r' "remote" "List remote branches" <|>
      flagWith Local Current 'c' "current" "Show current branch"

    scratchOpt :: Parser ScratchOption
    scratchOpt =
      ScratchTask <$> optionWith auto 's' "scratch-build" "TASKID" "Existing scratch build taskid" <|>
      flagWith ScratchBuild SkipScratch 'S' "no-scratch" "Skip scratch build"
