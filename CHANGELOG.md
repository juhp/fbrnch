# Changelog

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
