# Changelog

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
