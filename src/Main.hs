{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Maybe (fromMaybe)
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
import Cmd.Compare
import Cmd.Copr
import Cmd.Diff
import Cmd.FTBFS
import Cmd.Import
import Cmd.Install
import Cmd.ListBranches
import Cmd.ListPackages
import Cmd.Local
import Cmd.Merge
import Cmd.Mock
import Cmd.Override
import Cmd.Owner
import Cmd.Parallel
import Cmd.PkgReview
import Cmd.Prep
import Cmd.PullPush
--import Cmd.Repoquery
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.Reviews
import Cmd.Scratch
import Cmd.SideTags
import Cmd.Sort
import Cmd.SrcDeps
import Cmd.Status
import Cmd.Switch
import Cmd.Update
import Cmd.WaitRepo

import Bodhi (UpdateType(..),UpdateSeverity(..))
import Branches
import Common.System
import Git (CommitOpt(..))
import ListReviews
import Package (ForceShort(..), BCond(..))
import Paths_fbrnch (version)
import Types (SideTagTarget(..))

main :: IO ()
main = do
  setNoBuffering
  simpleCmdArgs (Just version) "Fedora branch building tool"
    "A tool to help with updating and building package branches https://github.com/juhp/fbrnch#readme" $
    subcommands
    [ Subcommand "clone" "Clone packages" $
      cloneCmd
      <$> cloneRequest
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
      <$> dryrunOpt
      <*> nopromptOpt
      <*> optional (optionWith auto 's' "skip-bumps" "NUM" "Max num of rebuild commits to ignore [default 0]")
      <*> switchWith 'a' "show-all" "List all commits [default first 20]"
      <*> optional (optionWith branchM 'f' "from" "BRANCH" "Branch to merge from [default newer]")
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
    , Subcommand "list-local" "List packages in branch" $
      listLocalCmd
      <$> maybeBranchPackages True
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
      <*> mergeOpt
      <*> optionalWith auto 'l' "skip-to-layer" "LAYERNO" "Skip the first N layers [default 0]" 0
      <*> optional (sidetagTargetOpt $ Just "or creates one for you (with 'fedpkg request-side-tag --base-tag')")
      <*> updateOpt
      <*> branchesPackages
    , Subcommand "sidetags" "List user's side-tags" $
      sideTagsCmd
      <$> switchLongWith "remove" "Remove one or more sidetags"
      <*> many branchArg
    , Subcommand "override" "Tag builds into buildroot override in Koji" $
      overrideCmd
      <$> dryrunOpt
      <*> optional (optionWith auto 'd' "duration" "DAYS" "Number of days until expiry [default 4]")
      <*> switchWith 'w' "no-wait" "Skip waitrepo step"
      <*> branchesPackages
    , Subcommand "waitrepo" "Wait for build to appear in Koji buildroot" $
      waitrepoCmd
      <$> dryrunOpt
      <*> waitfetchOpt
      <*> optional (sidetagTargetOpt Nothing)
      <*> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd
      <$> dryrunOpt
      <*> switchLongWith "stagger" "Stagger archs"
      <*> rebuildSrpmOpt
      <*> noFailFastOpt
      <*> optional archesOpt
      <*> many (sidetagTargetOpt Nothing)
      <*> optional (strOptionWith 'r' "ref" "COMMITHASH" "git commit to build")
      <*> branchesPackages
    , Subcommand "scratch-aarch64" "Koji aarch64 scratch build of package" $
      scratchCmdAarch64
      <$> dryrunOpt
      <*> rebuildSrpmOpt
      <*> switchWith 'X' "exclude-arch" "Exclude aarch64"
      <*> many (sidetagTargetOpt Nothing)
      <*> optional (strOptionWith 'r' "ref" "COMMITHASH" "git commit to build")
      <*> branchesPackages
    , Subcommand "scratch-x86_64" "Koji x86_64 scratch build of package" $
      scratchCmdX86_64
      <$> dryrunOpt
      <*> rebuildSrpmOpt
      <*> switchWith 'X' "exclude-arch" "Exclude x86_64"
      <*> many (sidetagTargetOpt Nothing)
      <*> optional (strOptionWith 'r' "ref" "COMMITHASH" "git commit to build")
      <*> branchesPackages
    , Subcommand "update-version" "Update package in dist-git to newer version" $
      updateCmd
      <$> switchWith 's' "sources-only" "Only update sources"
      <*> switchWith 'f' "force" "Download upstream sources even if they exist locally"
      <*> switchWith 'H' "allow-head" "For updating inside rebase"
      <*> maybeBranchPackages False
    , Subcommand "sort" "Sort packages in build dependency order" $
      sortCmd
      <$> sortDisplayOpt
      <*> optional rpmWithOpt
      <*> branchPackages
    , Subcommand "prep" "Prep sources" $
      prepCmd
      <$> optional prepPreOpts
      <*> switchWith 'v' "verbose" "show rpmbuild output"
      <*> maybeBranchPackages False
    , Subcommand "local" "Build locally" $
      localCmd
      <$> switchWith 'q' "quiet" "Hide the build.log until it errors"
      <*> switchWith 'd' "debug" "show the rpmbuild command"
      <*> optional forceshortOpt
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
      <*> switchWith 'i' "ignore-bumps" "Ignore pure release bumps"
      <*> optional diffFilterOpt
      <*> optional (optionWith anyBranchM 'w' "with-branch" "BRANCH" "branch")
      <*> maybeBranchPackages False
    , Subcommand "compare" "Show commits between branches" $
      compareCmd
      <$> switchWith 'l' "long" "show full commit log"
      <*> optional (strOptionWith 'i' "ignore" "SUBSTRING" "Matching substring to ignore")
      <*> anyBranchArg
      <*> anyBranchArg
      <*> manyPackages
    , Subcommand "src-deps" "List source package dependencies" $
      srcDepsCmd
      <$> switchWith 'r' "reverse" "Reverse dependencies"
      <*> branchPackages
    , Subcommand "mock" "Local mock build" $
      mockCmd
      <$> switchWith 'n' "dry-run" "Do not build (but creates srpm)"
      <*> optional nocleanOpt
      <*> switchWith 'w' "network" "Use network during build"
      <*> switchWith 's' "shell" "Enter chroot shell after building"
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
      <*> optional (optionLongWith branchM "from" "BRANCH" "Merge branch first")
      <*> optional forceshortOpt
      <*> many bcondOpt
      <*> switchWith 'r' "reinstall" "reinstall rpms"
      <*> switchWith 'a' "all-subpackages" "install all subpackages (default if none currently installed)"
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
      <*> switchWith '1' "first-line" "use first line of changelog"
      <*> switchWith 'a' "unstaged" "include unstaged changes"
      <*> manyPackages
    , Subcommand "pull" "Git pull packages" $
      pullPkgs
      <$> pullOpts
      <*> branchesPackages
    , Subcommand "fetch" "Git fetch packages" $
      fetchPkgs
      <$> manyPackages
    , Subcommand "push" "Git push packages" $
      pushPkgs
      <$> branchesPackages
    , Subcommand "owner" "List package owner(s)" $
      ownerCmd
      <$> manyPackages
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
      requestBranchesCmd
      <$> switchWith 'q' "quiet" "Quieter output"
      <*> optional (optionWith branchM 'r' "recurse-from" "BRANCH" "Add neighboring dependencies from branch")
      <*> mockOpt False
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
      <*> optional archesOpt
      <*> pkgArg "PROJECT"
      <*> branchesPackages
    , Subcommand "rename-rawhide" "Rename local 'master' branch to 'rawhide'" $
      renameMasterCmd
      <$> manyPackages
    , Subcommand "count" "Count number of living packages" $
      countCmd
      <$> maybeBranchPackages True
    , Subcommand "graph" "Output dependency graph" $
      graphCmd
      <$> switchWith 'o' "output" "Output graph in gv/dot format"
      <*> optional rpmWithOpt <*> maybeBranchPackages True
    -- , Subcommand "repoquery" "Repoquery branches (put repoquery options after '--')" $
    --   repoqueryCmd
    --   <$> branchesPackages
    , Subcommand "ftbfs" "Check FTBFS status" $
      ftbfsCmd
      <$> dryrunOpt
      <*> switchWith 'l' "short" "Only list packages"
      <*> optional (flagWith' (FtbfsUser Nothing) 'M' "mine" "Your packages"
                    <|>
                    FtbfsUser . Just <$> strOptionWith 'U' "user" "USER" "Bugzilla userid"
                    <|>
                    FtbfsSubstring <$> strOptionWith 's' "substring" "STRING" "Component substring")
      <*> maybeBranchPackages False
    ]
  where
    cloneRequest :: Parser CloneRequest
    cloneRequest =
      flagWith' (CloneUser Nothing) 'M' "mine" "Your packages" <|>
      CloneUser . Just <$> strOptionWith 'u' "user" "USER" "Packages of FAS user" <|>
      ClonePkgs <$> maybeBranchPackages True

    reviewShortOpt :: Parser Bool
    reviewShortOpt = switchWith 's' "short" "Only output the package name"

    reviewAllStatusOpt :: Parser Bool
    reviewAllStatusOpt = switchWith 'A' "all-status" "include all open states"

    reviewStatusOpt :: Parser ReviewStatus
    reviewStatusOpt =
      flagWith' ReviewUnApproved 'u' "unapproved" "Package reviews not yet approved" <|>
      flagWith' ReviewApproved 'a' "approved" "All open approved package reviews" <|>
      flagWith' ReviewWithoutRepoReq 'w' "without-request" "Approved package reviews without a repo request" <|>
      flagWith' ReviewRepoRequested 'r' "requested" "Approved package reviews with a pending repo request" <|>
      flagWith' ReviewRepoCreated 'c' "created" "Approved package reviews with a created repo" <|>
      flagWith' ReviewUnbranched 'B' "unbranched" "Approved created package reviews not yet branched" <|>
      flagWith ReviewAllOpen ReviewBranched 'b' "branched" "Approved created package reviews already branched"

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
    manyPackages =  many (pkgArg "PKGPATH...")

    -- somePackages :: Parser [String]
    -- somePackages = some (pkgArg "PKGPATH...")

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

    branchPackages :: Parser (Branch,[String])
    branchPackages =
      branchPkgs <$> some (pkgArg "[BRANCH] PKGPATH...")
      where
        branchPkgs :: [String] -> (Branch,[String])
        branchPkgs args =
          let (brs,pkgs) = partitionBranches args
          in case brs of
            [] -> error' "specify one branch"
            [br] -> (br,pkgs)
            _ -> error' $ "cannot have more than one branch: " ++ unwords (map show brs)

    maybeBranchPackages :: Bool -> Parser (Maybe Branch,[String])
    maybeBranchPackages oneplus =
      maybeBranchesPkgs <$>
      if oneplus
      then some (pkgArg "[BRANCH] PKGPATH...")
      else many (pkgArg "[BRANCH] [PKGPATH]...")
      where
        maybeBranchesPkgs :: [String] -> (Maybe Branch,[String])
        maybeBranchesPkgs args =
          let (brs,pkgs) = partitionBranches args
          in case brs of
            [] -> (Nothing, pkgs)
            [br] -> (Just br,pkgs)
            _ -> error' $ "cannot have more than one branch: " ++ unwords (map show brs)

    -- FIXME split later to prevent branches after packages
    branchesPackages :: Parser (BranchesReq, [String])
    branchesPackages =
      branchesReqPkgs <$> branchesOpt <*> many (pkgArg "BRANCH... PKGPATH...")
      where
        branchesReqPkgs :: Maybe BranchOpts -> [String] -> (BranchesReq, [String])
        branchesReqPkgs mbrnchopts args =
          let (brs,pkgs) = partitionBranches args
          in case mbrnchopts of
            Nothing -> (Branches brs, pkgs)
            Just req | null brs -> (BranchOpt req, pkgs)
                     | otherwise -> error' "cannot have branch option and branch list"

    prepPreOpts :: Parser PrepPre
    prepPreOpts =
      flagWith' PrepClone 'c' "clone" "Try to clone package repo first" <|>
      flagWith' PrepPull 'p' "pull" "Git pull before preping"

    rpmWithOpt :: Parser RpmWith
    rpmWithOpt =
      RpmWith <$> strOptionWith 'w' "with" "BCOND" "rpmspec --with option" <|>
      RpmWithout <$> strOptionWith 'W' "without" "BCOND" "rpmspec --without option"

    mockOpt brs = switchWith 'm' "mock" $ "Do mock build to test" ++ if brs then " branches" else ""

    archOpt :: Parser String
    archOpt = strOptionWith 'a' "arch" "ARCH[,ARCH].." "build for arch(s)"

    rebuildSrpmOpt = switchWith 'S' "rebuild-srpm" "rebuild srpm in Koji"

    mergeOpt =
      optional (flagWith' True 'm' "merge" "Merge without prompt" <|>
                flagWith' False 'M' "no-merge" "No merging")

    buildOpts =
      BuildOpts
      <$> mergeOpt
      <*> noFailFastOpt
      <*> optional (sidetagTargetOpt Nothing)
      <*> overrideOpt
      <*> waitrepoOpt
      <*> dryrunOpt
      <*> skipFetchOpt
      <*> updateOpt
      <*> useChangelogOpt
      <*> switchWith 'p' "by-package" "Build by each package across brs"
      <*> switchWith 'k' "allow-dirty" "Allow building from unclean git dir"
      where
        overrideOpt =
          optional (optionWith auto 'o' "override" "DAYS" "Create buildroot override for specified days: implies --wait-repo")
        waitrepoOpt =
          optional (flagWith' True 'w' "waitrepo" "Waitrepo for each build" <|>
                    flagWith' False 'W' "no-waitrepo" "Do not waitrepo for each build")
        useChangelogOpt =
          switchWith 'c' "changelog-notes" "Use spec changelog for Bodhi notes"

--    yesOpt = switchWith 'y' "yes" "Assume yes for questions"

    nopromptOpt = switchWith 'm' "no-prompt" "Merge without prompt"


    noFailFastOpt = switchWith 'f' "no-fail-fast" "Do not --fail-fast"

    excludeArch :: Parser String
    excludeArch = strOptionWith 'X' "exclude-arch" "ARCH[,ARCH].." "build without arch(s)"

    archesOpt :: Parser Archs
    archesOpt = Archs <$> some archOpt <|> ExcludedArchs <$> some excludeArch

    dryrunOpt = switchWith 'n' "dry-run" "Do not write (push, build, post, override)"

    skipFetchOpt = switchWith 'S' "skip-fetch" "Do not git fetch"

    updateOpt :: Parser (Maybe UpdateType, UpdateSeverity)
    updateOpt = updatePair <$> updatetypeOpt <*> updateSeverityOpt
      where
        updatetypeOpt =
          flagWith' Nothing 'U' "no-update" "Do not generate a Bodhi update" <|>
          Just <$> optionalWith auto 'u' "update-type" "TYPE" "security, bugfix, enhancement (default), newpackage, or template" EnhancementUpdate

        updateSeverityOpt =
          optionalLongWith auto "severity" "SEVERITY" "low, medium, high, urgent, (default: unspecified)" SeverityUnspecified

        updatePair :: Maybe UpdateType -> UpdateSeverity
                   -> (Maybe UpdateType, UpdateSeverity)
        updatePair (Just SecurityUpdate) SeverityUnspecified =
          error' "Security update requires specifying Severity"
        updatePair Nothing sev | sev /= SeverityUnspecified =
          error' "cannot have --severity with --no-update"
        updatePair (Just TemplateUpdate) sev | sev /= SeverityUnspecified =
          error' "Template update cannot have --severity"
        updatePair ty sv = (ty,sv)

    forceshortOpt =
      flagWith' ForceBuild 'f' "rebuild" "Rebuild even if already built" <|>
      flagLongWith' ShortCompile "short-compile" "Do rpmbuild --short-circuit -bc" <|>
      flagWith' ShortInstall 's' "short-install" "Do rpmbuild --short-circuit -bi"

    diffFormatOpt :: Parser DiffFormat
    diffFormatOpt =
      DiffContext <$> optionWith auto 'u' "unified" "CONTEXT" "Lines of context" <|>
      flagWith' DiffMinimal 'm' "minimal" "Minimize diff noise" <|>
      flagWith' DiffStatus 'n' "status" "Show diff --name-status" <|>
      flagWith' DiffStats 's' "stats" "Show diff --stat" <|>
      flagWith DiffDefault DiffQuiet 'q' "quiet" "Just output package name"

    diffFilterOpt :: Parser DiffFilter
    diffFilterOpt =
      DiffMatch <$> strOptionWith 'f' "filter" "PATTERN" "filter diff with pattern" <|> DiffNotMatch <$> strOptionWith 'F' "filter-not" "PATTERN" "negative filter for diff with pattern"

    diffWorkOpt :: Parser DiffWork
    diffWorkOpt =
      flagWith' DiffWorkStaged 'S' "staged" "Diff staged changes (git diff --cached)" <|>
      flagWith DiffWorkAll DiffWorkUnstage 'U' "unstaged" "Diff unstaged changes (git diff) [default is 'git diff HEAD']"

    commitOpts :: Parser CommitOpt
    commitOpts =
      CommitMsg <$>
      strOptionWith 'm' "message" "COMMITMSG" "commit message" <|>
      flagWith' CommitAmend 'A' "amend" "Amend commit"

    pullOpts :: Parser PullOpts
    pullOpts =
      PullOpts <$>
      switchWith 'l' "lenient" "Ignore non-git dirs and files" <*>
      switchWith 'f' "no-fetch" "Skip git fetch"

    buildByOpt = flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|> flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|> flagWith ValidateByRelease ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

    commandOpt = strOptionWith 'c' "cmd" "SHELLCOMMAND" "Shell command to run in $p"

    targetOpt :: Parser String
    targetOpt =
      checkNotRawhide <$> strOptionWith 't' "target" "TARGET" "Koji target"
      where
        checkNotRawhide "rawhide" = error' "'rawhide' is not a valid target!"
        checkNotRawhide t = t

    sidetagTargetOpt :: Maybe String -> Parser SideTagTarget
    sidetagTargetOpt mdesc =
      Target <$> targetOpt <|>
      flagWith' SideTag 's' "sidetag"
      ("Use existing branch side-tag to build" +-+ fromMaybe "" mdesc)

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

    nocleanOpt :: Parser NoClean
    nocleanOpt =
      flagWith' NoCleanBefore 'c' "no-clean" "Do not clean chroot before building"
      <|> flagWith' NoCleanAfter 'C' "no-clean-after" "Do not clean chroot after building"
      <|> flagWith' NoCleanAll 'A' "no-clean-all" "Do not clean chroot before or after building"
      <|> flagWith' MockShortCircuit 'S' "short-circuit" "Short circuit to install phase"

    sortDisplayOpt :: Parser SortDisplay
    sortDisplayOpt =
      flagWith' SortParallel 'p' "parallel" "Group dependent packages on separate lines"
      <|> flagWith' SortChain 'c' "chain" "chain-build output"
      <|> flagWith SortPlain SortLayers 'l' "layers" "output parallel layers"

    -- for waitrepo
    waitfetchOpt :: Parser WaitFetch
    waitfetchOpt =
      flagWith' WaitDirty 'k' "allow-dirty" "Allow unclean git repo" <|>
      flagWith WaitFetch WaitNoFetch 'F' "no-fetch" "Skip git fetch"
