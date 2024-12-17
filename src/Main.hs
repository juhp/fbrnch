{-# LANGUAGE CPP #-}

module Main (main) where

import Data.List.Extra (dropSuffix)
import Data.Maybe (fromMaybe)
import Distribution.Fedora.Branch (partitionBranches, readBranch)
#if !MIN_VERSION_simple_cmd_args(0,1,7)
import Options.Applicative (maybeReader, ReadM)
#endif
import SelectRPMs (existingStrategyOption, selectRpmsOptions)
import SimpleCmdArgs

-- commands
import Cmd.Autospec
import Cmd.Bugs
import Cmd.Build
import Cmd.Bump
import Cmd.Clone
import Cmd.Commit
import Cmd.Compare
import Cmd.CreateReview
import Cmd.Copr
import Cmd.Diff
import Cmd.Fetch
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
import Cmd.Prep
import Cmd.Pull
import Cmd.Push
--import Cmd.Repoquery
import Cmd.RequestBranch
import Cmd.RequestRepo
import Cmd.ReviewPackage
import Cmd.Reviews
import Cmd.Scratch
import Cmd.SideTags
import Cmd.Sort
import Cmd.SrcDeps
import Cmd.Status
import Cmd.Switch
import Cmd.Unpushed
import Cmd.Update
import Cmd.UpdateReview
import Cmd.WaitRepo

import Bodhi (UpdateType(..), UpdateSeverity(..), UpdateNotes(..))
import Branches
import Common.System
import Git (CommitOpt(..))
import ListReviews
import PkgReview
import RpmBuild (ForceShort(..), BCond(..))
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
      <$> optional (optionWith branchM 'b' "branch" "BRANCH" "branch")
      <*> cloneRequest
    , Subcommand "switch" "Switch branch" $
      switchCmd
      <$> verboseOpt "verbose output"
      <*> anyBranchArg
      <*> manyPackages
    , Subcommand "nvr" "Print name-version-release" $
      nvrCmd
      <$> branchesPackages
    , Subcommand "status" "Status package/branch status" $
      statusCmd
      <$> switchWith 'n' "no-fetch" "Do not git fetch to save time"
      <*> switchWith 'r' "reviews" "Status of reviewed packages"
      <*> switchWith 'l' "latest-commit" "Show the last commit"
      <*> branchesPackages
    , Subcommand "merge" "Merge from newer branch" $
      mergeCmd
      <$> dryrunOpt "Dry run (do not merge)"
      <*> switchLongWith "no-fetch" "Skip git fetch"
      <*> nopromptOpt
      <*> optional (optionWith auto 's' "skip-bumps" "NUM" "Max num of rebuild commits to ignore [default 0]")
      <*> switchWith 'a' "show-all" "List all commits [default first 20]"
      <*> optional (optionWith branchM 'f' "from" "BRANCH" "Branch to merge from [default newer]")
      <*> branchesPackages
    , Subcommand "unpushed" "Show unpushed commits" $
      unpushedCmd
      <$> switchWith 'c' "check-nvr" "Check NVR defined [default for < 10 pkgs]"
      <*> switchWith 'l' "latest" "Only show latest unpushed commit"
      <*> switchWith 'b' "bump" "Bump release (and commit) if no local commits"
      <*> branchesPackages
    , Subcommand "build" "Build package(s) in Koji" $
      buildCmd
      <$> buildOpts
      <*> branchesPackages
    , Subcommand "list" "List packages in pagure" $
      listCmd
      <$> forceOpt "Do not prompt if large number of results"
      <*> switchWith 'c' "count" "Print number of packages"
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
      <$> dryrunOpt "Dry run: do not build anything"
      <*> mergeOpt
      <*> optionalWith auto 'l' "skip-to-layer" "LAYERNO" "Skip the first N layers [default 0]" 0
      <*> optional (sidetagTargetOpt $ Just "or creates one for you (with 'fedpkg request-side-tag --base-tag')")
      <*> switchLongWith "must-push" "Error if no unpushed commits"
      <*> optionalWith auto 'd' "delay" "SECONDS" "Sleep delay between starting builds [default 4.0]" 4
      <*> updateOpt
      <*> branchesPackagesDesc "BRANCH... PKG1... [:] PKG2..."
    , Subcommand "sidetags" "List user's side-tags" $
      sideTagsCmd
      <$> dryrunOpt "Dry-run: no sidetag actions"
      <*> optional (flagLongWith' SidetagAdd "create" "Create one or more sidetags" <|>
                    flagLongWith' SidetagRemove "remove" "Remove one or more sidetags")
      <*> many branchArg
    , Subcommand "override" "Tag builds into buildroot override in Koji" $
      overrideCmd
      <$> dryrunOpt "Dry run: do not override"
      <*> overrideModeOpt
      <*> optional (optionWith auto 'd' "duration" "DAYS" "Number of days until expiry [default 4]")
      <*> switchWith 'w' "no-wait" "Skip waitrepo step"
      <*> branchesPackages
    , Subcommand "waitrepo" "Wait for build to appear in Koji buildroot" $
      waitrepoCmd
      <$> dryrunOpt "Dry run: do not wait"
      <*> pure False
      <*> waitfetchOpt
      <*> optional (sidetagTargetOpt Nothing)
      <*> branchesPackages
    , Subcommand "scratch" "Scratch build package in Koji" $
      scratchCmd
      <$> dryrunOpt "Dry run: do not build"
      <*> switchLongWith "stagger" "Stagger archs"
      <*> rebuildSrpmOpt
      <*> noFailFastOpt
      <*> allowHeadOpt
      <*> optional archesOpt
      <*> many (sidetagTargetOpt Nothing)
      <*> optional scratchSourceOpt
      <*> branchesPackages
    , Subcommand "scratch-aarch64" "Koji aarch64 scratch build of package" $
      scratchCmdAarch64
      <$> dryrunOpt "Dry run: do not build"
      <*> rebuildSrpmOpt
      <*> allowHeadOpt
      <*> switchWith 'X' "exclude-arch" "Exclude aarch64"
      <*> many (sidetagTargetOpt Nothing)
      <*> optional scratchSourceOpt
      <*> branchesPackages
    , Subcommand "scratch-x86_64" "Koji x86_64 scratch build of package" $
      scratchCmdX86_64
      <$> dryrunOpt "Dry run: do not build"
      <*> rebuildSrpmOpt
      <*> allowHeadOpt
      <*> switchWith 'X' "exclude-arch" "Exclude x86_64"
      <*> many (sidetagTargetOpt Nothing)
      <*> optional scratchSourceOpt
      <*> branchesPackages
    , Subcommand "update-sources" "Download and update newer sources" $
      updateCmd True
      <$> forceOpt "Download upstream sources even if they exist locally"
      <*> switchWith 'H' "allow-head" "For updating inside rebase"
      <*> maybeBranchPackages False
    , Subcommand "update-version" "Update package in dist-git to newer version" $
      updateCmd
      <$> switchWith 's' "sources-only" "Only update sources"
      <*> forceOpt "Download upstream sources even if they exist locally"
      <*> switchWith 'H' "allow-head" "For updating inside rebase"
      <*> maybeBranchPackages False
    , Subcommand "sort" "Sort packages in build dependency order (default format: chain-build)" $
      sortCmd
      <$> sortDisplayOpt
      <*> optional rpmWithOpt
      <*> branchPackages
    , Subcommand "prep" "Prep sources" $
      prepCmd
      <$> optional prepPreOpts
      <*> verboseOpt "show rpmbuild output"
      <*> switchWith 'd' "deps" "require deps to be installed"
      <*> allowHeadOpt
      <*> maybeBranchPackages False
    , Subcommand "local" "Build locally" $
      localCmd
      <$> quietOpt "Hide the build.log until it errors"
      <*> debugOpt "show the rpmbuild command"
      <*> jobsOpt
      <*> optional forceshortOpt
      <*> many bcondOpt
      <*> branchesPackages
    , Subcommand "srpm" "Build srpm" $
      srpmCmd
      <$> forceOpt "regenerate even if spec older than existing srpm"
      <*> maybeBranchPackages False
    , Subcommand "srpm-spec" "Show the spec file in an srpm" $
      srpmSpecCmd
      <$> switchWith 'd' "diff" "Compare with current spec file"
      <*> some (strArg "SRPM")
    , Subcommand "diff" "Diff local changes" $
      diffCmd
      <$> debugOpt "use package headers"
      <*> switchWith 'o' "spec-only" "Only diff spec file"
      <*> diffWorkOpt
      <*> diffFormatOpt
      <*> switchWith 'i' "ignore-bumps" "Ignore pure release bumps"
      <*> many diffFilterOpt
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
      <*> many (strOptionWith 'D' "define" "'MACRO DEF'" "Define RPM macro")
      <*> branchPackages
    , Subcommand "mock" "Local mock build" $
      mockCmd
      <$> dryrunOpt "Do not build (but creates srpm)"
      <*> optional nocleanOpt
      <*> switchWith 'N' "network" "Use network during build"
      <*> optional (flagLongWith' ShellOnly "shell-only" "Skip mock build" <|>
                    flagWith' BuildShell 's' "shell" "Enter chroot shell after building")
      <*> optional (optionWith branchM 'r' "root" "BRANCH" "Mock config to use")
      <*> optional archOpt
      <*> branchesPackages
      -- was install-deps
    , Subcommand "builddeps" "Install package build dependencies" $
      installDepsCmd
      <$> maybeBranchPackages False
    , Subcommand "install" "Build locally and install package(s)" $
      -- FIXME drop --shortcircuit from install?
      installCmd
      <$> quietOpt "Quiet rpmbuild output"
      <*> switchWith 'R' "recurse" "build and install missing deps packages"
      <*> optional (optionLongWith branchM "from" "BRANCH" "Merge branch first")
      <*> jobsOpt
      <*> optional forceshortOpt
      <*> many bcondOpt
      <*> switchWith 'r' "reinstall" "reinstall rpms"
      <*> switchLongWith "no-build" "do not (re-)build (install built rpms)"
      <*> switchLongWith "ignore-builddeps" "do not install builddeps"
      <*> selectRpmsOptions
      <*> optional existingStrategyOption
      <*> maybeBranchPackages False
    , Subcommand "not-installed" "Packages not installed locally" $
      notInstalledCmd
      <$> maybeBranchPackages False
    , Subcommand "bugs" "List package bugs" $
      bugsCmd
      <$> optional (strOptionWith 's' "summary" "KEY" "Search for bugs containing keyword")
      <*> manyPackages
    , Subcommand "bump" "Bump release for package" $
      bumpCmd
      <$> dryrunOpt "Dry run: do not bump"
      <*> switchWith 'l' "local" "Use local origin rather than checking latest koji"
      <*> optional (strOptionWith 'm' "message" "COMMITMSG"
                    "Specify commit message")
      <*> optional (strOptionWith 'c' "changelog" "CLOGENTRY"
                    "Override changelog entry [default: rebuild]")
      <*> branchesPackages
    , Subcommand "commit" "Git commit packages" $
      commitCmd
      <$> dryrunOpt "Dry run: do not commit"
      <*> optional commitOpts
      <*> switchWith '1' "first-line" "use first line of changelog"
      <*> switchWith 'a' "unstaged" "include unstaged changes"
      <*> manyPackages
    , Subcommand "pull" "Git pull packages" $
      pullPkgs
      <$> optional pullOpts
      <*> branchesPackages
    , Subcommand "fetch" "Git fetch packages" $
      fetchPkgs
      <$> manyPackages
    , Subcommand "push" "Git push packages" $
      pushPkgs
      <$> dryrunOpt "Dry run: do not push"
      <*> switchLongWith "no-fetch" "Skip git fetch"
      <*> optional (strOptionWith 'r' "ref" "COMMITHASH" "git commit to push")
      <*> branchesPackages
    , Subcommand "owner" "List package owner(s)" $
      ownerCmd
      <$> manyPackages
    , Subcommand "bzusers" "Search bugzilla users" $
      bzusersCmd
      <$> strArg "NAME"
    , Subcommand "create-review" "Create a Package Review request" $
      createReviewCmd
      <$> forceOpt "create a review even if one exists already"
      <*> optional scratchOpt
      <*> mockOpt False
      <*> manyPackages
    , Subcommand "update-review" "Update a Package Review" $
      updateReviewCmd
      <$> optional scratchOpt
      <*> mockOpt False
      <*> optional (strArg "SPECFILE")
    , Subcommand "review-package" "Run fedora-review on a package Review Request bug" $
      reviewPackage
      <$> switchWith 'i' "interactive" "Download and check package review without fedora-review mock build"
      <*> optional (pkgArg "PACKAGE/BZID")
    , Subcommand "reviews" "List package reviews" $
      reviewsCmd
      <$> reviewShortOpt
      <*> reviewAllStatusOpt
      <*> optional
      (flagLongWith' Nothing "assigned" "Package reviewer" <|>
       Just <$> strOptionLongWith "assignee" "BZUSER" "Package review submitter")
      <*> optional
      (flagLongWith' Nothing "submitted" "Submitted reviews [default if no assignee specified]" <|>
       Just <$> strOptionWith 's' "submitter" "BZUSER" "Package review submitter")
      <*> optional (strOptionWith 'p' "pattern" "PKGPREFIX" "Package pattern prefix")
      <*> reviewStatusOpt
    , Subcommand "request-repos" "Request dist git repo for new approved packages" $
      requestRepos
      <$> mockOpt True
      <*> reviewAllStatusOpt
      <*> switchWith 's' "skip-request-check" "Skip check for existing fedora-scm-requests issue"
      <*> switchWith 'r' "resubmit" "Re-request repo"
      <*> branchesPackages
    , Subcommand "import" "Import new approved created packages from bugzilla review" $
      importCmd False
      <$> switchLongWith "existing" "Use an existing repo"
      <*> mockOpt True
      <*> branchesPackages
    , Subcommand "request-branches" "Request branches for approved created packages" $
      requestBranchesCmd
      <$> quietOpt "Quieter output"
      <*> switchWith 'r' "reviews" "Request branches for package reviews"
      <*> optional (optionWith branchM 'R' "recurse-from" "BRANCH" "Add neighboring dependencies from branch")
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
      <$> dryrunOpt "Dry run: do not build"
      <*> (flagWith' ListChroots 'l' "list-chroots" "Show project chroots" <|>
           flagLongWith' CoprNew "new" "Create new copr repo" <|>
           flagWith CoprBuild CoprMonitor 'm' "monitor" "Show project chroots")
      <*> forceOpt "build even if existing n-v-r"
      <*> optional buildByOpt
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
      <$> dryrunOpt "dry run"
      <*> switchWith 'l' "short" "Only list packages"
      <*> optional (flagWith' (FtbfsUser Nothing) 'M' "mine" "Your packages"
                    <|>
                    FtbfsUser . Just <$> strOptionWith 'U' "user" "USER" "Bugzilla userid"
                    <|>
                    FtbfsSubstring <$> strOptionWith 's' "substring" "STRING" "Component substring")
      <*> maybeBranchPackages False
    , Subcommand "autospec" "Convert package to use rpmautospec" $
      autospecCmd
      <$> forceOpt "Refresh changelog file to current"
      <*> manyPackages
    , Subcommand "move-artifacts" "Move old rpm artifacts into rpmbuild dirs" $
      moveArtifactsCmd
      <$> switchWith 'd' "delete" "Remove duplicate artifacts"
      <*> manyPackages
    ]
  where
    cloneRequest :: Parser CloneRequest
    cloneRequest =
      flagWith' (CloneUser Nothing) 'M' "mine" "Your packages" <|>
      CloneUser . Just <$> strOptionWith 'u' "user" "USER" "Packages of FAS user" <|>
      CloneGroup <$> strOptionWith 'g' "group" "GROUP" "Packages of package group" <|>
      ClonePkgs <$> somePackages

    reviewShortOpt :: Parser Bool
    reviewShortOpt = switchLongWith "short" "Only output the package name"

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
    pkgArg lbl = dropSuffix "/" <$> strArg lbl

    manyPackages :: Parser [String]
    manyPackages =  many (pkgArg "PKGPATH...")

    somePackages :: Parser [String]
    somePackages = some (pkgArg "PKG...")

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
            _ -> error' $ "cannot have more than one branch:" +-+ unwords (map showBranch brs)

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
            _ -> error' $ "cannot have more than one branch:" +-+ unwords (map showBranch brs)

    branchesPackages :: Parser (BranchesReq, [String])
    branchesPackages = branchesPackagesDesc "BRANCH... PKGPATH..."

    -- FIXME split later to prevent branches after packages
    branchesPackagesDesc :: String -> Parser (BranchesReq, [String])
    branchesPackagesDesc desc =
      branchesReqPkgs <$> branchesOpt <*> many (pkgArg desc)
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
    archOpt = strOptionWith 'a' "arch" "ARCH" "build for arch(s)"

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
      <*> dryrunOpt "Dry run: do not merge/push/build"
      <*> skipFetchOpt
      <*> updateOpt
      <*> optional notesOpt
      <*> switchWith 'p' "by-package" "Build by each package across brs"
      <*> switchLongWith "stash" "git stash before fetching and building"
      where
        overrideOpt =
          optional (optionWith auto 'o' "override" "DAYS" "Create buildroot override for specified days: implies --wait-repo")
        waitrepoOpt =
          optional (flagWith' True 'w' "waitrepo" "Waitrepo for each build" <|>
                    flagWith' False 'W' "no-waitrepo" "Do not waitrepo for each build")
        notesOpt =
          flagWith' NotesChangelog 'c' "changelog-notes" "Use spec changelog for Bodhi notes" <|>
          NotesText <$> strOptionLongWith "notes" "NOTES" "Bodhi update notes"

--    yesOpt = switchWith 'y' "yes" "Assume yes for questions"

    nopromptOpt = switchWith 'm' "no-prompt" "Merge without prompt"


    noFailFastOpt = switchWith 'f' "no-fail-fast" "Do not --fail-fast"

    excludeArch :: Parser String
    excludeArch = strOptionWith 'X' "exclude-arch" "ARCH" "build without arch"

    archesOpt :: Parser Archs
    archesOpt = Archs <$> some archOpt <|> ExcludedArchs <$> some excludeArch

    dryrunOpt desc = switchWith 'n' "dry-run" desc <|>
                     switchLongWith "dryrun" "alias for --dry-run"

    debugOpt = switchWith 'd' "debug"

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
      flagWith' ShortCompile 'c' "short-compile" "Do rpmbuild --short-circuit -bc" <|>
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

    pullOpts :: Parser PullOpt
    pullOpts =
      flagWith' PullLenient 'l' "lenient" "Ignore non-git dirs and files" <|>
      flagWith' PullNoFetch 'f' "no-fetch" "Skip git fetch" <|>
      flagWith' PullStash 's' "stash" "Stash local changes" <|>
      flagWith' PullRebase 'r' "rebase" "Git pull instead of fetch"

    buildByOpt =
      flagWith' SingleBuild 'S' "single" "Non-progressive normal single build" <|>
      flagWith' BuildByRelease 'R' "by-release" "Builds by release" <|>
      flagWith' ValidateByArch 'A' "by-arch" "Build across latest release archs first (default is across releases for primary arch)"

    commandOpt = strOptionWith 'c' "cmd" "SHELLCOMMAND" "Shell command to run in $p"

    targetOpt :: Parser String
    targetOpt = strOptionWith 't' "target" "TARGET" "Koji target"

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
      ScratchTask <$> optionLongWith auto "scratch-task" "TASKID" "Existing scratch build taskid" <|>
      flagWith' SkipScratch 'S' "no-scratch" "Skip koji scratch build" <|>
      flagWith' ScratchBuild 's' "scratch" "Do koji scratch build without prompt"

    scratchSourceOpt :: Parser ScratchSource
    scratchSourceOpt =
      ScratchSRPM <$> strOptionLongWith "srpm" "SRPM" "SRPM to use" <|>
      ScratchRef <$> strOptionWith 'r' "ref" "COMMITHASH" "git commit to build"

    nocleanOpt :: Parser NoClean
    nocleanOpt =
      flagWith' NoCleanBefore 'c' "no-clean" "Do not clean chroot before building"
      <|> flagWith' NoCleanAfter 'C' "no-clean-after" "Do not clean chroot after building"
      <|> flagWith' NoCleanAll 'A' "no-clean-all" "Do not clean chroot before or after building"
      <|> flagWith' MockShortCircuit 'S' "short-circuit" "Short circuit to install phase"

    sortDisplayOpt :: Parser SortDisplay
    sortDisplayOpt =
      flagWith' SortParallel 'p' "parallel" "Group dependent packages on separate lines"
      <|> flagWith' SortLayers 'l' "layers" "output parallel layers"
      <|> flagWith SortChain SortPlain 's' "separated" "Dependent grouping separated by empty lines"

    -- for waitrepo
    waitfetchOpt :: Parser WaitFetch
    waitfetchOpt =
      flagWith' WaitDirty 'k' "allow-dirty" "Allow unclean git repo" <|>
      flagWith WaitFetch WaitNoFetch 'F' "no-fetch" "Skip git fetch"

    overrideModeOpt :: Parser OverrideMode
    overrideModeOpt =
      flagWith' OverrideList 'l' "list" "List active override(s)" <|>
      flagWith OverrideCreate OverrideExpire 'X' "expire" "Expire override(s)"

    verboseOpt :: String -> Parser Bool
    verboseOpt = switchWith 'v' "verbose"

    quietOpt :: String -> Parser Bool
    quietOpt = switchWith 'q' "quiet"

    allowHeadOpt = switchLongWith "allow-head" "allow detached HEAD"

    forceOpt = switchWith 'f' "force"

    jobsOpt = optional (optionWith auto 'j' "jobs" "NUM" "Max processes in rpmbuild")
