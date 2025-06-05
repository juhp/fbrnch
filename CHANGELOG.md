# Changelog

## 1.7 (2025-06-05)
- support epel10.x minors with fedora-releases-0.3.0
- 'merge': now done from origin for consistency
- targetMaybeSidetag: prompts to choose which sidetag
- 'autospec': no duplicate %changelog line
- 'branches': add --only-dead option
- 'branches': improve the --current error for multiple branches
- 'branches': improve the prefixing for multiple packages
- 'build': newline after git push
- 'build': update latest tagged check to include epel-testing; print all
- 'build': use target branch to determine nvr
- 'bump': complete rework of logic (--strict replaces --local)
- 'clone': add --dryrun
- 'clone': now supports globbing of packages
- 'copr': order chroots by branch (release): so epel10 > epel9
- 'copr': rename i686 to i386
- 'fetch': fix --lenient was no-op
- 'import': better push prompt labeling
- 'import': now also waits for package to be setup in koji
- 'install': add --yes
- 'install': perform also if given a rpm subpackage selection
- 'install': select-rpms-0.3.0 adds --error-existing
- 'install': skip ":" separator
- 'list': refactor with countListPackages
- 'mock': add experimental --install mode (for single package)
- 'parallel': better sidetag setup with warnings
- 'parallel': more color output/tweaks
- 'parallel': offer to retry failures or continue with waitrepo for layer
- 'parallel': print nvr earlier and only latest commits
- 'parallel': reduce default delay to 1s between triggering builds
- 'parallel': skip "koji-tool builds --tail" and always show task url for fail
- 'pull': drop stash pop to avoid making conflicts
- 'push': check local branch exists
- 'repoquest-repo': does not reprint full bug for import
- 'request-branches': long todo comment about catching API key expiry
- 'request-branches': now also checks for commit access via group membership
- 'request-branches': request all branches/pkgs before waiting for koji pkgs
- 'review-package': defaults to interactive, use --full for fedora-review
- 'scratch': do not create a new sidetag!
- 'sidetags': listings are now sorted
- 'switch': add --lenient option to avoid error for non-existent branch
- 'uninstall': new experimental command to remove a package NVR
- 'update-sources': partition sources by isArchiveFile
- 'waitrepo': add --no-nvr option for generic wait-repo
- Bodhi: better error handling with bodhiUpdatesEither refactor
- Branches gitCurrentBranchWarn: offer to rename master to rawhide
- Branches listOfBranches: print branch EOL warning when < 7 days left
- Branches systemBranch: support CPE_NAME
- Package isArchiveFile: ".sig" files are ascii
- RpmBuild getSources: use exists for checkCompression of uncompressed file
- RpmBuild: check/install compression tool before downloading tarball
- buildRPMs: backup build.log's with cp -p
- checkSourcesMatch: match sources filenames wrapped with parens
- mergeBranch: add nofetch switch to avoid refetching
- new 'tag-build-to-sidetag': tags branch's nvr into a sidetag
- recognize .crate as archive file (#70)
- refPrompt: accept "origin" and require ref to over 3 chars
- refPrompt: append newline if prompt less than 1 line
- withPackagesByBranches: git fetch earlier to get any new branches
- withPackagesByBranches: switch back branch before unstash

## 1.6.2 (2025-03-05)
- 'diff': use git diff's builtin filtering
- 'fetch': add --lenient to skip non dist-git directories
- 'local': --detached-head
- 'parallel': do not waitrepo for final layer
- 'parallel': koji-tool tail of failed builds and end with task url
- 'parallel': only print "parallel layer" is more than one pkg in layer
- 'request-branches': now waits for package to be listed for branch buildtag
- 'request-branches': wait 30s before checking for package added to tag
- 'request-repos': error if no reviews found
- 'sidetags': --tagged lists tagged builds
- 'unautospec': new command to remove %autochangelog and/or %autorelease
- 'update-sources': now takes optional version, replacing 'update-version'
- 'waitrepo': use kojiWaitRepoNVR output and timing
- kojiWatchTask: print task url after failure log
- Krb module replaced by fedora-krb library: handles different FAS userid (#67)
- rename --allow-head to --detached-head
- targetMaybeSidetag: strict parameter to enforce branch tag match
- targetMaybeSidetag: warn/prompt if target does not match branch
- withPackagesByBranches: print package header earlier before fetching

## 1.6.1 (2024-12-17)
- 'build','parallel': waitrepo now just uses "koji wait-repo --request"
- 'build': now respects --waitrepo
- 'local','install': new --jobs option for _smp_ncpus_max
- waitrepo is timed
- Koji targetMaybeSidetag: print sidetags if more than one

## 1.6 (2024-12-13)
* update to fedora-releases-0.2.0 (showBranch and branchDestTag)
* kojiWaitRepo: now uses request-repo to trigger newrepo
- 'build': use --notes to set Bodhi update notes text (#59)
- 'clone' now uses fedpkg for better compatibility (#62)
* 'copr' refactoring and fix existingChrootBuilds to prevent rebuilds
- 'create-review': --force to create a new "duplicate" review (#53)
* 'create-review': include FAS id if can be determined (#42)
* 'import': prompt whether to build or only push (#47)
- 'install': add --existing-only, --skip-existing, --no-reinstall options from select-rpms-0.2 (ported from koji-tool --install)
- 'parallel': koji-tool tail of failed build.log if < 3 packages in layer
* 'request-repo': now offers to import the new repo immediately
- 'request-repo': only post comment if prompt input
* 'review-package': allow --interactive also for local review
- 'review-package': summarize long rpmlint error output
- 'reviews --created': now uses REL_PREP state
- 'scratch-x86_64','scratch-aarch64': --exclude-arch no longer fast fails
- 'sidetag': add --create
- 'update': remove %autorelease -b bump when updating version
- Common.System: override cmdFull to avoid weird lazy IO crash
- Merge: gitMergeable now returns Maybe Bool for reverse ancestry
- Package pkgNameVerRelDist: add autorelease strict parameter for rpmBuild
- RpmBuild buildRequires: improve dyn BRs handling
- dropped all head and tail functions usage
- fix merging from new unfetched branch
- fixup "Push and build" prompt text, which is no longer promptEnter (#47)
- gitPush: say message and drop "done"
- sentence-case commit and changelog messages (#38)
- split CreateReview and UpdateReview out of PkgReview
- targetMaybeSidetag: now maps "rawhide" to its dist_tag

## 1.5 (2024-08-17)
* fedora-releases (using bodhi) replaces fedora-dists (pdc): explicit imports
- 'bugs': also include product in prefix
- 'bump': fix handling of autorelease
- 'clone': --group allows cloning all of group's packages (#46)
- 'clone': print no of repos
- 'command' --if-output: drop trailing newline
- 'commit': add --dryrun
- 'commit': require --message for %autochangelog
- 'compare': no longer reorders branches, checks their existence
- 'copr --new': create a new copr project
* 'copr': no longer rebuilds nvr's unless --force
* 'copr': does not resubmit existing successful or in-progress nvr's
- 'copr': add --monitor
- 'copr': fix bug where only first two source packages got built
- 'copr': sort chroots by arch; distless srpm
- 'diff' can now take multiple filters
- 'ftbfs': use --details with koji-tool
- 'import': if no branches requested, skip requestPkgBranches
* 'install': use select-rpms library refactored from koji-tool
- 'list': check number of results and prompt if >1000 unless --force
* 'parallel': support chain-build args with colon interspersed layers
- 'parallel': retry kojiWaitTask
- 'parallel': --must-push to enforce commit bumps
- 'parallel': koji-tool use --children to get failed buildArch
- 'parallel': only do parallelBranches build if more than 1 branch
* 'parallel': better concurrent output with say library
* 'prep','update': use cwd for %_sourcedir
- 'prep': --allow-head
- 'pull': add --rebase
- 'push': print log relative to origin
- 'repoquery': add testing repos (not yet using fedora-repoquery)
- 'request-repo': retry listing existing scm requests
- 'request-repos': add --skip-request-check and rename --retry to --resubmit
* 'review-package --interactive': new lighter review mode without mock
* 'reviews': new --assignee, --submitted and --submitter options
* 'sort': default to chain-build output
- 'sort': skip getDynSourcesMacros for dead.package
- 'status': add --latest-commit
- 'unpushed --bump': now uses 'bump' code
- 'unpushed': add --check-nvr and do not print every "dead package"
* 'update-sources': git add patches
- 'update-review': end with bug url
- '--dryrun' is now an alias for '--dry-run'
- Package pkgNameVerRelDist: use fedpkg verrel if autorelease
- RpmBuild: checkSourcesMatch yesNo prompt for git amend
- fix stack build of HsOpenSSL with gcc14 -Wno-incompatible-pointer-types
- fix systemBranch to determine rawhide correctly via distBranch
- generateSrpmNoDist: also warn if generated srpm is different to prediction
- generateSrpmNoDist: fallback to fedpkg for autospec
- kojiWatchTask: use koji-tool tasks --children to get failed task
- module refactorings from Local, PkgReview, PullPush, Status
- more consistent prompt handling of 'commit'/'update'/'review' messages
- require rpmbuild-order 0.4.12 to order rust crates and rubygems
- withPackagesByBranches: git fetch earlier to get new branches

## 1.4 (2024-05-03)
- buildSrpm: filter .src.rpm filename
- checkSourcesMatch: prompt before amending and abort if still dirty
- getSources: handle no srcdir correctly when checking for sources
- pkgNameVerRel: only fedpkg verrel for autorelease if dist-git
- use rpm-nvr NVR type everywhere
- 'build': fail earlier if dirty
- 'build': replace --allow-dirty with --stash
- 'commit': don't look for unstaged local changes if --unstaged
- 'commit': don't print singleline commit msg unnecessarily
- 'commit': unquote macros from changelog diff for commit msg
- 'copr': don't use localBranchSpecFile rawhide
- 'copr','scratch': doc --exclude-arch only takes a single arch
- 'create-review': set bug url field with spec url
- 'ftbfs': include Modified bugs
- 'import': use Yes/no prompt to push
- 'install': now defaults to verbose build output with a --quiet option
- 'local','install': -c short option for --short-compile
- 'local','install': re-backup previous buildlog if bigger than current
- 'merge': add newline after unmerged commits
- 'mock': can now build from non-dist-git
- 'move-artifacts': allow dirty
- 'move-artifacts': improve arch/rpms dir logic
- 'parallel': assert git is clean before pushing
- 'parallel': don't error if no builds to push to Bodhi
- 'parallel': no longer wait for bodhi to transition to request-testing
- 'parallel': use dependencyLayersRpmOpts from rpmbuild-order-0.4.11
- 'parallel': use koji-tool tasks to get failed build.log url
- 'parallel': use pluralException to special case zero
- 'pull': --stash does git stash and pop around git pull
- 'request-branches': filter for processed pending branches
- 'request-repos': print bz url before proceeding
- 'review-package': do not assume cwd matches pkg name
- 'scratch' --allow-head detached with gitSwitchBranchVerbose
- 'sort': distRpmOptions defines %fedora/%rhel %fcN/%elN
- 'switch' --verbose

earlier changes from November:

- 'build','parallel': checkSourcesMatch now offers to 'update-sources'
- 'build': only check tags if overriding
- 'builddeps': renamed from 'install-deps'
- 'commit': if 2 lines then add second without - prefix
- 'copr': small refactor to get more precise build log url
- 'create-review': assert spec file exists
- 'import': add --existing to allow importing to an existing repo
- 'mock': make it possible to run for non-distgit dir
- 'parallel': print sidetag info early
- 'parallel': append ':' after package in bodhi notes
- 'parallel': branches should be dist-git
- 'parallel': various layer output/prompt improvements
- 'request-branches': add --reviews and change --recurse-from -r to -R
- 'create/update-review': improve buildAndUpload messages to depend on scratch
- Koji targetMaybeSidetag: newline before printing new sidetag
- RpmBuild buildRPMs: define `_rpmautospec_release_number` for autorelease
- RpmBuild buildRPMs: prepend %distprefix to disttag
- RpmBuild: fix local handling of dyn BRs
- RpmBuild: handle %autorelease locally with distOpt and autoreleaseOpt
- targetMaybeSidetag now has a create switch

## 1.3.3 (2023-10-19)
- 'bump': add --dry-run
- 'copr': track pkg name to output build results url on failure
- 'create-review','update-review': prompt to offer scratch build (#43)
- 'diff': allow origin as alias for origin/<branch>
- 'import': encode url from bz comment if needed (#40)
- 'import': prefix bug# with rhbz in commit (#38)
- 'install': add --no-build to install existing built rpms
- 'parallel': --delay to override default inter-package pause [default 3s]
- 'parallel': use pkg name in changelog instead of nvr
- 'parallel': print if no more layers
- 'parallel': try to make Bodhi update prompt more obvious
- 'request-repo': avoid head crash on firstname (#45)
- 'request-repo': no longer post request url to avoid duplication
- 'scratch': with --exclude-arch respect ExcludeArch: fields
- 'unpushed': add --bump
- 'unpushed': output improvements for --latest and dead.package/missing
- 'update-sources': alias for "update-version --source-only"
- 'update-version': only warn about branch if dist-git
- 'update-version': rpm prep with --nodeps
- 'update-version': take .gpg and .tgz file for lookaside archive!
- Krb: loop fkinit until okay
- Main: --dry-run options now better described
- RpmBuild: prevent srpm creation from being interrupted (eg ctrl-c)
- gitFetchSilent: use \r to hide "git fetching..."
- kojiBuild: remove "Task info: " prefix before koji task url
- kojiWatchTask: do not hardcode koji-tool path
- pkgNameVerRel: use fedpkg to determine correct %autorelease (#39)
- refPrompt: do not trim input for more precision

## 1.3.2 (2023-05-30)
- prompts now support line-editting thanks to simple-prompt-0.2 using haskeline
- 'parallel', 'sort', 'graph': use getDynSourcesMacros
- 'parallel': include no of layers in "more package layers" message
- 'parallel': output sidetag
- 'prep': default to --nodeps
- 'request-branches': output owners (to ask) if no permission
- 'request-branches': committers can also request branches
- 'scratch': print target for srpm build (only)
- 'src-deps': add --define 'MACRO DEF'
- 'update-version': munch spectool patch filenames too
- Bodhi only accepts update notes <= 10000 characters now
- Git refPrompt: also accept y/yes
- Koji targetMaybeSidetag dryrun: do not append "-dryrun" to buildtag
- Merge: newline after local commits
- Package cleanChangelog: append a newline

## 1.3.1 (2023-04-09)
- check for %autorelease more carefully
- buildRequires: fix dynamic BRs with getSources and space after %_srcrpmdir
  (reported by kiilerix)

## 1.3 (2023-04-08)
- new 'autospec' command converts packages to use rpmautospec
- new 'move-artifacts' command moves rpmbuild artifacts into dirs (--delete)
- new 'srpm-spec' command shows or diffs srpm specfiles
- new 'unpushed' command shows unpushed commits; and --latest option
- new 'bzusers' command searches users in bugzilla
- 'build': check for Bodhi update before koji tags
- 'build': do not repeat header when merging
- 'build': improve logic and prompt for unpushed check
- 'bump': add --changelog override and handle %autorelease
- 'copr': intersperse newlines between packages
- 'diff': --debug option to print package header
- 'diff': handle missing and non-release branches
- 'install': add --ignore-builddeps (for existing built rpms)
- 'install': ignore dead.package's
- 'install': show dnf commands with sudoLog and cmdN sudo
- 'merge': --no-fetch option
- 'mock': add --arch option
- 'mock': new --shell-only option which skips build
- 'override': default to ./ when no pkg path given
- 'parallel': if single layer, don't output layer #
- 'parallel': improve sidetag update transition messages, now waits 90s
- 'parallel': output tweaks for more packages/layers and existing nvr
- 'parallel': prompt whether to continue after failure
- 'parallel': reverse the package order for update changelog
- 'parallel': be quieter when many parallel packages
- 'push': --no-fetch option and also print header
- 'push': allow specifying a ref
- 'rename-rawhide': now pulls to get latest
- 'scratch': add --srpm option to build existing srpm
- 'status': add pkg/branch prefix before latest log
- Bugzilla: update checkRepoCreatedComment for fedora-admin automation
- Copr branchRelease: do not error for EPELNext!
- buildRPMs: backup the build.log file to build.log.prev
- buildRPMs: print NVR instead of package name
- changeLogPrompt: no trailing newline after showing changelog
- gitPush replaces gitPushSilent
- kojiWaitRepo: knowntag assurance parameter for build and parallel
- kojiWatchTask: use koji-tool to get build.log tail on failure
- use simple-prompt

## 1.2.1 (2022-11-23)
- 'install': if dnf install fails, include command in error message
- 'override': check for kerberos ticket
- 'override': new --list and --expire option subcommands
- 'parallel': change "0 jobs left in layer" message to "end of layer"
- 'parallel': print package header for merge
- 'parallel': wait for sidetag update to transition to request testing
- 'request-branches': check package owner and admins: drop unordered-containers
- 'sidetags': ensure krb ticket
- 'update-version': now commits sources too
- 'update-version': prompt rather than warn if not updating rawhide
- Common plural: use 'no' for zero
- Git: fix conflictPrompt to handle long hashes correctly
- Prompt: use show for unprintable characters
- mergeBranch: print package branch header
- new RpmBuild module for rpm/build related functions moved from Package
- newerMergeable may now include the newer branch
- require bugzilla-redhat-1.0.1 since b.r.c dropped Bug see_also field (juhp/hsbugzilla#18)
- use logMsg when waiting for repo

## 1.2 (2022-11-12)
- Bodhi: for template file prompt instead of using fedpkg update
- Bodhi: updates need to be comma separated (#36, Otto Liljalaakso)
- Git gitMergeable: new logic for detecting older branch ahead/diverged
- Git getNewerBranch: restore epel9 merge inheriting from fedora
- Git: rename gitShortLog to gitOneLineLog with long hash format
- Main: fix overlapping -s/-S options
- Package buildRPMs: with tee return error if build failed
- Package pkgNameVerRel: seq disttag for early error
- Package: add withPackagesBranch, withPackagesMaybeBranch always ZeroOrOne
- Prompt refPrompt: allow "n" for no
- SrcDeps: factor out srcDeps (for request-branches --recursive)
- bodhiUpdate: take comma-separated nvrs and check all the updates (#36)
- 'build': initial support for --sidetag
- buildRPMs: add --noclean switch for 'local' builds
- 'compare': add --ignore to filter out matching commits
- 'diff': fold --quiet into DiffFormat and add --ignore-bumps
- getNewerBranch now only returns next branch
- getRequestedBranches now only returns new branches out of specified
- gitFetchSilent,gitFetchSilent': quiet option
- gitFetchSilent: newline before filtered output
- gitSwitchBranch': do not warn about missing branch if quiet
- 'install': also count down when recursing
- 'install': offer to merge and add --from to override branch
- 'install': only update installed subpackages, unless --all-subpackages
- kojiBuild': don't log if no output
- 'local' --debug: outputs command (refactor buildRPMs)
- 'local': add --short-compile and rename --short-circuit to --short-install
- long-only options need simple-cmd-args-0.1.8
- 'merge': --skip-bumps up to N (trivial) commits & --show-all; Commit and Patch
- 'merge': add --dryrun
- 'merge': fix getting newer branch when no --from
- mergeBranch,gitMergeOrigin: no more gratuitous merge rebasing
- 'mock': add simple --short-circuit option for install stage
- new 'owner' command: prints owner and admins of package(s)
- new 'fetch' command
- new 'list-local' command: lists packages in branch with spec file
- new 'src-deps' command: wrapper of rpmbuild-order (r)deps
- 'override','waitrepo': print message before waiting
- 'parallel' build: respect --no-merge
- 'parallel': also accept "n" for "no" update
- 'parallel': also time parallel branches dir
- 'parallel': display package's number and layer
- 'parallel': do not prompt for update when update disabled
- 'parallel': dryrun for merge and delay until bodhiSidetagUpdate
- 'parallel': filter out rawhide from parallel branch package builds update (#36)
- 'parallel': formatting - use +-+, plural, don't color "is already"
- 'parallel': no longer offers sidetag removal
- 'parallel': only write "in layer" if there are layers
- 'parallel': print pkg brnch header before merging
- 'request-branches/repos': check 100 fedora-scm-requests for duplicate
- 'request-branches': add --quiet to suppress "exists" messages
- 'request-branches': add --recurse-from BRANCH
- 'request-branches': do not fetch if remote branch known
- 'request-branches': improve --quiet help
- 'request-branches': output if exists, again
- 'scratch' builds now support multiple (serial) targets
- 'scratch','waitrepo': now also accept --sidetag
- 'scratch-x86_64','scratch-aarch64': add --exclude-arch to invert arch selection
- 'sidetags': add --remove option
- srcDeps uses depsGraphDeps from rpmbuild-order-0.4.10
- targetMaybeSidetag: add dryrun parameter
- 'update': renamed to update-version
- 'update-version': check/download missing patches with spectool
- 'update-version': warn if branch

## 1.1.2 (2022-07-30)
- add scratch-x86_64 and scratch-aarch64 shortcut commands
- bodhiUpdate now takes multiple builds: used for parallelBranches (#30)
- build: add --allow-dirty
- commit: add all if nothing staged; remove initial "- "
- commit: default to changelog even if multiline
- compare: check branch exists and only print package if log output
- create-review: Fix for different username/fasid (@LyesSaadi, #34)
- install: abort if installation failed
- install: print already installed packages on separate lines
- kojiWaitRepo: add quiet arg for many parallel builds
- listReviewsAll: use bz api key
- merge: do not error if target branch does not exist
- parallel: create changelog for parallel package builds
- parallel: now takes same merge opts as build (#30)
- parallel: timeIO layers and parallel packages
- parallel: use branch to determine autoupdate or prompt for update edit
- pull: switch to given branch, add --no-fetch, show header when several pkgs
- rename Log to Compare and logCmd to compareCmd
- rename withPackageByBranches to withPackagesByBranches with HeaderShow & count
- request-branches: check user in access_users to avoid bad requests
- request-branches: print package for existing branch
- scratch: add --stagger to stagger archs
- scratch: only check pushed if release branch
- use rpmbuild-order-0.4.8 (needs simple-cmd-0.2.3)
- waitrepo: --allow-dirty and --no-fetch
- waitrepo: use timeIO to time the operation
- Branches getRequestedBranches: exclude existing branches
- Git gitMergeable: only check newer branch if local
- Git gitMergeable: warn if branch ahead of newer branch
- Package buildRPMs: also create build.log for silent build
- Package buildRPMs: log start of build also for verbose
- Package withPackagesByBranches: mention pkg name when spec names differs
- Parallel bodhiSidetagUpdate: revert to edit update and then delete sidetag

## 1.1.1 (2022-05-30)
- copr: time builds
- commit: fix getting log from additional changelog message
- merge: --from to specify which branch to merge
- Package getSources: only check 'sources' if dist-git
- Package getSources: install compression tool if missing
- Package buildRPMs: use shellBool to send stderr also to tee
- request-branches: print out url per branch
- require rpm-nvr 0.1.2 for better NVR name validation
- initial support for epel-next (#29)
- Package builtRpms: no longer assume in cwd for dist-git
- build: now possible to skip pushing an update at prompt with 'no'
- main help: add readme url
- build: add --skip-fetch option
- Git isPkgGitRepo: fix handling of forks (#33)
- InterleaveOutput: fix cmdSilent' to error on failure
- parallel: add --merge (#30)
- fix -s/-S ambigious for build and parallel
- parallel: unlock update after sidetag removal
- parallel: waitrepo for sidetag before starting to build
- parallel: get buildtag from koji to determine basetag for sidetag
- parallel: allow branch options like --all-fedora/--all-epel

## 1.1 (2022-04-30)
- parallel: show target as early as possible
- status: warn if branch does not exist rather than erroring
- new 'ftbfs' command: lists FTBFS bugs
- build,parallel: add --template to support bodhi update templates
- local: --quiet option to suppress the rpmbuild output
- sort: add --chain (#25) and --layers
- Prompt: reject input with escape sequence chars
- Package checkLookasideCache: accept old md5 hashes in sources too
- merge: fetch newer branch if not local
- merge: accept HEAD in conflict prompt
- parallel: more output improvement tweaks
- Package getSources: canonicalize %_sourcedir
- Package: make sure rpmautospec is installed if needed
- Package: support %autochangelog
- build: fix --no-waitrepo short option to be '-W'
- Bugzilla readIniConfig: include ini filename in parser error
- build: extractBugReference require 7 digits RH bug id
- Package buildRPMs: only override dir macros if not dist-git
- Prompt: only allow printable chars in input (703a575)
- Branches listOfBranches: return branches for non-distgit
- build: --override now takes number of days (#31)
- scratch now requires a target or branch for non dist-git
- build no longer offers to merge unmergeable
- override no longer git fetches again when doing wait-repo
- switch: fix branch handling for multiple packages
- print duration of builds, etc
- parallel package now returns to original branch like build
- override: make sure a spec file exists
- commit: change '-u' to '-a' (all) and '-a' to '-A' (amend)
- request-branch: don't use full path for package name
- prep, srpm, local, scratch, mock: respect _builddir, _rpmdir, _srcrpmdir
- Main: most commands take PKGPATH not PACKAGE name
- build,parallel: add --severity (#32)
- build: changelog lines can contain multiple bzs
- build: simplify update type logic to detect pkgreview again and print
- request-branch: always print urls
- Git: isPkgGitRepo now ignores dist-git fork
- Package buildRequires: use installMissingMacros for dyn BR
- clone: --branch option is now an optional arg
- mock: --no-clean* fix and tweak

## 1.0.0 (2022-02-21)
- rpm's _sourcedir is now acts as a source cache directory:
  sources are in the package dir, but may be hardlinks to _sourcedir
- getSources checks compression integrity to re-download partial tarballs
  and missing sources entries now only prompt once
- initial support for %autorelease (#24)
- 'build': consolidate nvr output
- 'build': skip branch if already built
- 'build': --changelog-notes to use spec changelog for Bodhi notes
- 'bump': new --local switch
- 'commit': --first-line to only use 1st line from changelog
- 'commit': --staged to only commit staged changes
- 'commit': extract incremental commit message from diff
- 'copr': --exclude-arch option
- 'count': new command to count packages with .spec files
- 'diff': new --status, --filter and --filter-not options
- 'log' renamed to 'compare'
- 'mock': use system arch, not hardcoded to x86_64
- 'parallel': more compact and precise output
- 'parallel': only sleep 3s for job start and 1s when job still pending
- 'parallel': error with pending package layers and number of packages
- 'parallel': new --skip-to-layer option for restarting after failure
- 'prep': --clone option
- 'prep': install any required srpm macros (golang, rust, fonts)
- 'reviews': output one line per package
- 'switch': informative error when detached and use "git switch"
- 'update': --source-only to update sources without version bump
- 'update': add --force to re-download sources from upstream
- 'update': check changelog versions before bumping
- 'update': use --allow-head for updating a detached rebase
- error with package name when no .spec file found
- Merge: rebase after non-ancestor merge
- ListReviews: use anonymous bugzilla queries
- checkOnBranch and gitCurrentBranch now prompt if detached HEAD

- update to bugzilla-redhat-1.0 and use API key bzApiKeySession
- 'build': build by branch by default instead of by package, if >1 package
- 'build': --[no-]waitrepo option changes default behavior with autoupdate
- 'commit': replace --staged with --unstaged
- 'commit': tweak changelog filter to "+- " prefix
- 'copr': handle --all-fedora and --all-epel
- 'list': use pagure-hs release with new Object API
- 'mock': add --no-clean-all and --shell to enter chroot after building
- 'parallel': output enhancements
- 'prep': add --pull option (alternative to --clone)
- 'prep': add --verbose for prep output
- 'pull': add --lenient to ignore non-git dirs/files
- 'pull': handle old master branch better
- 'push': new git push command
- 'rename-master' renamed to 'rename-rawhide'
- 'request-branches': output branch names with newline
- 'scratch': print package header by default
- 'sort': add --parallel option to group dependent packages
- 'update': only run spectool -g once with --force
- 'update': only sed edit sources if it exists
- 'update': improve the mver/pkg/dirty logic
- Bodhi: fix for aeson-2.0
- Bugzilla: getBzUser save email id in ~/.config/fbrnch/bugzilla
- Bugzilla: many queries now use anonymous session
- Krb: factor out klistEntryFedora adding maybeFasIdFromKrb
- Krb: klist errors if no tickets
- Package: add checkLookasideCache to checkSourcesMatch
- Package: builtRpms: check in %_rpmdir if not distgit
- Package: fix lookaside check url
- Package: handle Dynamic BRs locally
- Package: refactor checkCompression
- allow anonymous cloning of packages
- decouple bodhi-hs, copr-hs, pagure-hs, pdc-hs
- fedora-dists-2.0 was released: remove the submodule
- kojiWatchTask: don't backtrace for task failure
- latest fedora-dists uses cached-json-file & has better releaseBranch err
- readme: note https checkouts currently assumed anonymous (#27)
- require rpm-nvr-0.1.1 for VerComp bugfix

## 0.9.1.1 (2021-08-01)
- getSources: fix mixup between sources and patches
- 'reviews': add --pattern for package prefix

## 0.9.1 (2021-07-29)
- 'prep': cleaner output
- 'reviews': pre-sort by bug id
- 'scratch': --ref to specify a commit other than HEAD to build
- 'scratch': correct --no-fast-fail to --no-fail-fast
- 'build': --no-fast-fail now works
- 'install': header if multiple pkgs and reinstall if force
- 'install': better output and show number of packages
- 'install': dnf builddep for missing (non-local) deps
- 'install': add --verbose to show buildlog
- 'command': --continue/-k (rename -k/--compact to -1)
- 'command': --compact outputs package name on same line
- 'command': also continue through errors for --if-output
- 'command': ignore dead.package's completely
- 'diff': fix help string for --stats
- 'request-repos': newline between packages
- 'request-branches': print package and/or branch, when multiple
- 'branches': use readBranch to avoid error for fc6
- buildRPMs: pipe to tee for build.log
- Package: getSources now checks for and downloads patches too
- Package: buildRPMs timestamp for build
- Package: installDeps simplify output and use --quiet
- Package: improve output messages for prep and rpm build failures
- Package: getSources check if fedpkg available when pulling sources

## 0.9 (2021-05-30)
- 'override' --duration and run waitrepo after all overrides
- new 'waitrepo' command
- 'create-review','rename-master': take no branches
- branchPrompt now correctly defaults to two branches
- prompts now reset stdin (#20)
- 'build': add --no-merge and rename --no-prompt to --merge
- 'copr': native watch-build
- 'install-deps': use --skip-unavailable for dnf buildeps
- 'install': switches to specified branch; allow --recurse & --rebuild
- 'branches': add --current and support --missing
- 'request-repo': show review comments and prompt to continue
- merge handling improvements
- 'scratch': display nvr before building
- 'parallel': native koji waitTask
- 'sidetags': default to all user's sidetags
- 'create-review','update-review': --scratch-build TASKID option
- Koji: timeouts to handle for network connection failures
- 'parallel': error if no branch given
- gitFetchSilent: show git reponame instead of dirname
- new 'graph' command renders dependency graph using graphviz
  (using rpmbuild-order-0.4.5)
- fix handling of optional branch args
- merge: improve output when not mergeable
- srpm: regenerate if any sources newer than existing srpm file

## 0.8.0 (2021-04-08)
- 'request-repos' can now take branch args and --mock option (#18)
- 'parallel': only override and waitrepo if more layers
- extend dryrun to bodhiCreateOverride, putBugBuild, bodhiUpdate, kojiWaitRepo
- drop the restriction of no packages inside a pkg dir (#19)
- 'switch': drop git clean requirement to prevent error when on branch
- 'parallel': use parallelBranches for single package arg
- 'request-branch': allow request with closed pkg review
- major refactor of branch-package args handling together with branch options
- 'copr': new --list-chroots option for project
- 'command': skip dead.package's
- 'build': clearer bodhi note prompt
- 'install': fix conflicting short options for --recurse and --reinstall

## 0.7.3 (2021-03-23)
- 'scratch': don't get sources for pushed git build
- changes/improvements to package header output
- 'merge','build': show/query diverging newer commits
- 'merge': add --no-prompt option
- Build: refine the "still in testing" logic to check testing repo with prompt
- Koji: offer to resubmit build on error
- 'request-repos' now prompts for branching and
  'import' skips offering branching if they exist
- 'import': add --mock option for checking branches (#18)
- add 'not-installed' command: lists packages not installed locally at all
- Bugzilla: don't bother reprinting comment after posting it
- 'rename-master': renamed from 'master-rename'
- 'srpm': add --force option
- `local --force` (or --short): ignore existing built rpms
- 'command': option to hide package header if no output
- 'create-review': fix error for non-existent dir
- 'parallel': don't sleep 5s for dryrun

## 0.7.2 (2021-02-27)
- 'install': --recurse to install missing neighboring deps
- 'request-repo': offer to request branches too
- 'override': --dryrun
- Bodhi overrides: error if failed; use 4 days
- 'build': no longer override the last of built packages
- 'review-package': new experimental command
- 'local','install': print package name when build fails
- 'copr': abort on failure
- 'sort': only switch branch for dist-git
- bunch of other tweaks and smaller fixes

## 0.7.1 (2021-02-09)
- fix package review urls and also run rpmlint on .spec
- 'master-rename' improvements
- workaround spectool src download bug introduced with 0.7.0.1
- Fedora web service API libraries are now internal libraries

## 0.7.0.1 (2021-02-05)
- Srpm generation: error if spectool fails to download source tarball

## 0.7.0 (2021-02-05)
- reworked branch/pkg arg processing
  which allows `branches --remote` to work without a repo
- 'local'/'install': --with/--without bcond options
- Koji: exit if aborting incomplete build (prevents premature bugzilla comment)
- Bugzilla: updateBug result is Array not Object
- 'build': for a new package use rpm Summary & Url for Bodhi note
- 'build': offer to create a Bodhi update for an already built candidate nvr
- new 'update' command: highly experimental package version updating
- 'diff' now works for a different branch
- 'request-branches' now posts pagure urls to package review too
- 'import' prompts for branching after build
- 'build': --no-prompt replaces --merge
- Git: more readable shortlogs
- 'parallel': only use --background if >5 packages in layer (#17)
- Master branches are now Rawhide
- new 'master-rename' command: renames package master branches to rawhide

## 0.6.9 (2021-01-17)
- PkgReview: use fasid for ssh (#16)
- Bugzilla: separate commentBug from updateBug

## 0.6.8 (2021-01-16)
- build: check bodhi client new update success more carefully
- Bugzilla: correctly check that bug update succeeded
- new 'list' command to list packages from pagure
- branches: add --remote option
- update to latest lts-16.29

## 0.6.7 (2020-12-23)
- Bugzilla: don't conflate POST comment with PUT update
- build/install: allow no branch arg for current directory
- request-repos: prompt for reviewer thanks
- request-repos: added --all-states for Modified
- copr: only output buildargs when build fails

## 0.6.6 (2020-12-17)
- Bugzilla: use POST again for comments...
- copr: print chroots when building

## 0.6.5 (2020-12-03)
- support git worktrees (experimental)
- branches: fix --missing output for given branch and --skip-dead
- git fetching now outputs new branches
- build: only wait-repo if overriding or autoupdate
- koji: improve uploading message
- mock and scratch: --dryrun
- add --all-fedora and --all-epel branch options (#15)

## 0.6.4 (2020-11-12)
- Bugzilla: fix updating of bugs and check for error
- import: offer to request-branches after build
- branches: can now take branch args and --missing option
- request-repo: thank reviewer by first name

## 0.6.3 (2020-10-21)
- new 'branches' command lists package's branches
- copr: fix running in a package dir
- copr: revert restarting failed watch
- mock: fix running in a package dir
- mock: --network option
- improve errors for commands that only take zero or one branches

## 0.6.2 (2020-10-20)
- build: fix bodhi update options error
- merging to latest epel now works (from oldest active fedora branch)
- status: fixed error on new branch
- request-repo: thank reviewer by name
- copr: watch now restarts on a net failure

## 0.6.1 (2020-10-17)
- build, parallel: request testing state for bodhi updates
- bugs: --summary to filter by a phrase
- reviews: --user to list reviews from another user
- reviews: --assigned-to to list bugs by reviewer
- reviews: filter out "Rename Request" and "Re-Review Request" prefixes
- copr: remove trailing / from project name

## Below are pre-release changes from the copr package rpm:

* Fri Oct  9 2020 Jens Petersen <petersen@redhat.com> - 0.6-1 (34223fd)
- accept spec filepaths as package args (#12)
- native Koji wait-repo (#14)
- parallel: now requires --sidetag or --target unless development branch
  - offers bodhi update from side-tag
  - lists koji sidetags natively
  - checks not in pkg dir when multiple packages
- mock: chain multiple package builds
  - Add --no-clean and --no-clean-after
- new 'log' command for comparing branch logs
- new 'override' command
- new 'sidetags' command lists user's sidetags per branch
- Bugzilla: fix body comments not getting posted with PUT (#13)
- status: --no-fetch option
- bump: only if not latest in koji
- merge: assert still in branch after prompt input
- Git short log: use reference format
- copr: detect non-existent project
- Bodhi overrides now display url

* Sun Sep 20 2020 Jens Petersen <petersen@redhat.com> - 0.5-3 (e32fac9)
- new 'bump' command to bump package release
- 'parallel': new --side-tag option to use user's current branch side-tag
- 'build': print nvr and git push as late as possible only if nvr new
- 'bugs' command can now take multiple packages
- wait-repo first without nvr to avoid "nvr not latest" warning
- remove single "- " prefix from changelog for commit
- fix merge prompt text

* Fri Sep 11 2020 Jens Petersen <petersen@redhat.com> - 0.5-2 (1d44bd3)
- copr: error if no chroots
- build,parallel: improve review post/autoupdate and override logic
- reviews: add --branched filter, --short, and --all-status
- scratch: experimental --dry-run and output message before srpm upload
- scratch: add --exclude-arch
- commit: improve checking for changelog
- build: --no-update option
- fetching and switch output pkgdir name
- fix Bugzilla utf8 encoding corruption (when posting reviews, etc)
- new 'command' for running an arbitrary shell command across packages
- diff: --spec-only and --with-branch (use origin if branch not local)
- improve branch error message if package paths/dir wrong
- fix error when detecting autoupdate for master

* Tue Sep  1 2020 Jens Petersen <petersen@redhat.com> - 0.5-1 (41ed229)
- major change: branches are now args preceding package args
- new 'nvr' command
- 'build' doesn't override for Bodhi create_automatic_updates branches
- 'switch'ing to a new branch should work now without explicit pulling
- disallow Fedora branch newer than latest branched release
- 'commit' without -m will try to use rpm changelog
- 'commit' also works in a pkg dir now
- 'create-review' and 'request-branches' can now handle multiple packages

* Fri Aug 21 2020 Jens Petersen <petersen@redhat.com> - 0.4-3 (39edd49)
- check active branches after option parsing (#10)
- `reviews --approved` now limits to NEW, ASSIGNED and POST
- `status -B` now displays new branches
- latest fedora-dists lib uses ~/.fedora/product-versions-2.json

* Sun Aug 16 2020 Jens Petersen <petersen@redhat.com> - 0.4-2 (3d8a9e5)
- rebuild with correct updated cabal.project git deps

* Fri Aug 14 2020 Jens Petersen <petersen@redhat.com> - 0.4-1 (45a590b)
- local commands now work for non-release branches (eg module branches)
- scratch: build by default for branch target
- Koji: check taskstate even if koji watch-task appears to succeed (#9)
- parallel: parallel branch building in a package dir (#6)
- diff: don't git fetch every time
- Koji: abort waiting (do not loop) if task canceled
- fedora-dists: avoid caching PDC error instead of product list
- status: recognize epel testing tags
- Package: warn rather than error if spec filename differs from pkg/dir
- build: don't push Bodhi update for create_automatic_updates releases
- be more careful with creating and comparing NVRs
  - fixing handling for bootstrap bcond (#5)
- parallel: also experimental --dryrun

* Sun Aug  9 2020 Jens Petersen <petersen@redhat.com> - 0.3-5 (98370b7)
- fix handling of failed bugzilla login
- uses http-query for fedora web api libs and bugzilla responses

* Fri Aug  7 2020 Jens Petersen <petersen@redhat.com> - 0.3-4
- strip executable

* Fri Aug  7 2020 Jens Petersen <petersen@redhat.com> - 0.3-3 (a07a33b)
- add 'install-deps' (builddep) command
- experimental --exclude-branch option

* Fri Aug  7 2020 Jens Petersen <petersen@redhat.com> - 0.3-2 (5a44f81)
- local/prep: revert back from using "." for rpmbuild macros dir options

* Thu Aug  6 2020 Jens Petersen <petersen@redhat.com> - 0.3-1 (acb05de)
- install: don't build if existing rpm files newer than spec
  and add --rebuild and --short-circuit options
- use absolute paths for sudo and dnf everywhere
- add 'copr' build command (ported from juhp/cobrnch)

* Wed Aug  5 2020 Jens Petersen <petersen@redhat.com> - 0.2-9 (7499a63)
- improve mock results dir paths like fedpkg

* Wed Aug  5 2020 Jens Petersen <petersen@redhat.com> - 0.2-8 (9462af2)
- support .git file repos also for prepping and building

* Wed Aug  5 2020 Jens Petersen <petersen@redhat.com> - 0.2-7 (d65baee)
- fix srpm generation when _sourcedir is user undefined

* Tue Aug  4 2020 Jens Petersen <petersen@redhat.com> - 0.2-6 (70e945c)
- local: print uninstalled deps
- sort: --with/--without options (rpmbuild-order-0.4.2) (#5)
- support absorbed git submodules (#8)
- build: experimental --dry-run option
- build: Bodhi --update-type option (#7)

* Tue Aug  4 2020 Jens Petersen <petersen@redhat.com> - 0.2-5 (4784a63)
- build now does git fetch and merge of origin
- clone: output package names to show progress
- wait-repo's now show datestamp
- build: maybe override and waitrepo when build already complete
- sort/parallel: update to rpmbuild-order-0.4.1 which also shows any subcycles

* Wed Jul 29 2020 Jens Petersen <petersen@redhat.com> - 0.2-4 (f7a009d)
- parallel: only override when no target or not stable
- parallel: fixed to switch to branch
- update to rpmbuild-0.4.0 release with bugfixes:
  (Provide Name and parse package name dirs with a dot)

* Wed Jul 29 2020 Jens Petersen <petersen@redhat.com> - 0.2-3 (570d531)
- parallel: do override for built package if not tagged (#3)
  - reported by QuLogic
- fix pull command and check for clean working dir
- latest rpmbuild-order fixes a recent regression
- be more lenient when package is in a old branch
- generate a basic manpage with help2man

* Thu Jul 23 2020 Jens Petersen <petersen@redhat.com> - 0.2-2 (9b8982d)
- further simply the option/arg parsing for better error messages

* Wed Jul 22 2020 Jens Petersen <petersen@redhat.com> - 0.2-1 (d8c9a66)
- build/merge/status by default now only act on the current branch
  and require a branch option when more than one package
- use -B or --all-branches to act on all branches like before
- read Koji for correct buildtag for wait-repo
- ignore sources file when not dist-git

* Wed Jul 22 2020 Jens Petersen <petersen@redhat.com> - 0.1-12
- bash completions

* Tue Jul 21 2020 Jens Petersen <petersen@redhat.com> - 0.1-11 (5d9e3af)
- mock: add --root option (takes a branch)

* Tue Jul 21 2020 Jens Petersen <petersen@redhat.com> - 0.1-10 (3a2c181)
- build/parallel: check no existing koji task which is not yet building
- parallel: rpmbuild-order now preserves dir paths to packages
- scratch: allow multiple --arch options

* Sun Jul 19 2020 Jens Petersen <petersen@redhat.com> - 0.1-9 (8492abd)
- 'build' now does wait-repo between packages
- 'build' always checks sources file up to date
- 'build' can now rejoin started builds
- new 'parallel' build command for building packages in dependency layers
- new 'diff' command for checking changes across many packages
- new 'commit' command for committing changes across many packages
- prep now prints nvr
- 'install' handles reinstalls correctly (when only some subpackages installed)
- ignore remote branches other than origin

* Wed Jul 15 2020 Jens Petersen <petersen@redhat.com> - 0.1-8 (e5dd3f3)
- local --short-circuit
- interleaved output for prep and local errors

* Fri Jul 10 2020 Jens Petersen <petersen@redhat.com> - 0.1-7 (981285c)
- build: allow pushing/building before current HEAD
- scratch: add --rebuild-srpm option (default is --no-rebuild-srpm)
- status: allow dirty working dir

* Sat Jul  4 2020 Jens Petersen <petersen@redhat.com> - 0.1-6 (254130a)
- scratch: --arch option and don't get sources too early
- build: drop --scratch
- srpm and mock commands

* Fri Jul  3 2020 Jens Petersen <petersen@redhat.com> - 0.1-5 (7d0e70d)
- 'scratch' build command
- 'build' options --override and --no-fail-fast

* Thu Jul  2 2020 Jens Petersen <petersen@redhat.com> - 0.1-4 (cdeaffb)
- create-review/updatereview: now run rpmlint and optionally mock

* Thu Jul  2 2020 Jens Petersen <petersen@redhat.com> - 0.1-3 (6fd8b0a)
- local, install: install deps

* Wed Jul  1 2020 Jens Petersen <petersen@redhat.com> - 0.1-2 (4612be2)
- prep, local, install: pull down sources now
- add Requires for client tools

* Wed Jul  1 2020 Jens Petersen <petersen@redhat.com> - 0.1-1 (4fe8239)
- initial package
