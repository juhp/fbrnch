# fbrnch (fedora branch) packager tool

[![GPL-2+ license](https://img.shields.io/badge/license-GPL--2+-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/fbrnch/workflows/build/badge.svg)](https://github.com/juhp/fbrnch/actions)
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)

A tool to help Fedora Packagers build package branches.

Fedora developers use a lot of time building packages across releases
and workflow for adding new packages, etc.
fbrnch was made to help (semi-)automate common workflows to save time
and effort.

fbrnch is distributed under the GPL license version 2 or later.

## Description
`fbrnch` is a Fedora Packager client which tries to automate some common tasks
like:

- merging and building a package across release branches
- automated parallel builds of sets of packages in dependency order
- easy scratch and mock builds
- progressive copr builds
- creating, updating and listing package reviews
- requesting new repos and branches
- importing new packages and updating packages
- rename master branches to rawhide

and more.

## Usage

### Creating new packages/branches

#### Creating a new package
```
$ fbrnch create-review [my-new-package]
```
This will create (or update) an srpm, run rpmlint,
then upload it to fedorapeople, perform a scratch build,
and open a Review Request in Bugzilla (similar to fedora-create-review).

#### Update a package review
```
$ fbrnch update-review [my-new-package]
```
Similar to create-review: it uploads the updated files to fedorapeople
and posts the updated package urls to the open package review.

#### List open package reviews
To list one's open package reviews:
```
$ fbrnch reviews
```
They can be filtered by status with various options like
`--approved` or `--created`.

One can also search for the review(s) of a specific package with:
```
$ fbrnch find-review package-name
```

#### Request repos
Once a review has been approved
```
$ fbrnch request-repos
```
will request repos for approved package(s) and offer to request branches.

#### Import a new package
After the repo has been created
```
$ fbrnch import
```
will clone the repo and offer to import the srpm
directly from the latest url in the approved package review,
which can then be built directly into Koji Rawhide
and the package review is updated.

#### Request branches
If you prefer you can request branches after the repo is created
or package imported with
```
$ fbrnch request-branches
```
which will be prompt for which branches you want, unless already given.

Optionally a mock build per branch can be done first.


### Cloning and switching branch
Clone one or more packages:
```
$ fbrnch clone package ...
```
There are also options to clone all one's packages or another user's packages.

One can change the branch of one or more packages:
```
$ fbrnch switch f36 [package] ...
```

You can also git pull over packages:
```
$ fbrnch pull [package] ...
```

### List packages and branches
You can list packages in dist-git pagure with
```
$ fbrnch list PATTERN
```
with globbing.

Branches of a package can be listed:
```
$ fbrnch branches -B [package] ...
```
You can use the `--remote` option if you don't have the package checked out.

### Package status
```
$ fbrnch status -B [package]
```
outputs information about the status of each branch.
The status command can also be used with `--reviews`
to check the build status of new packages.

You can output package's nvr's from local git:
```
$ fbrnch nvr -B [package]
```

and list package bugs:
```
$ fbrnch bugs [package]
```

### Commit, Merging and Building in Koji
You can commit to the current branch:
```
$ fbrnch commit
```
It uses any rpm changelog, or you can pass `-m "..."` or amend with `-a`.

You can merge branches with:
```
$ fbrnch merge f35 package
```
which will offer to merge f36 (or some of it) into f35.

Merging can also be done together with building:
```
$ fbrnch build f36 package
```
will ask if you want to merge newer commits from a newer branch,
then push and build it.

If the branch is also already pushed and NVR built it will be skipped.
Branch builds are pushed to Bodhi.

You can also build all branches:
```
$ fbrnch build -B package
```
or all fedora branches:
```
$ fbrnch build -F package
```
or all epel branches:
```
$ fbrnch build -E package
```

Scratch builds can also be done:
```
$ fbrnch scratch rawhide
```
optionally a different koji `--target` can be given.

You can sort packages by build dependency order:
```
$ fbrnch sort rawhide package1 package2 package3 package4 ...
```

### Local commands
```
$ fbrnch prep rawhide package
```

Build locally:
```
$ fbrnch local
```
this works in the current package dir like other commands,
installing any missing dependencies with `sudo dnf builddep`.

Or one can specify the path to the package.

Locally build and install:
```
$ fbrnch install package1 package2 package3 ...
```

You can use:
```
$ fbrnch install-deps [package]
```
to only install the dependencies of a package.

Use
```
$ fbrnch rename-master [package]
```
to rename an old master branch locally to rawhide.

### Parallel building
fbrnch can sort packages automatically and build them in parallel
in Koji in dependency layers (using low-priority background builds
to avoid grabbing too many Koji resources).

```
$ fbrnch parallel --sidetag rawhide pkg-x pkg-y pkg-z pkg-xy pkg-xy-z
```
generates a sidetag and builds a list of packages there
in parallel ordered by build dependencies.

Except for rawhide using a sidetag is required,
so you need to manage and select them
if using more than one per branch with `--target`.

### Other commands
There are more commands like `copr` and `graph`.

```
$ fbrnch --version
1.1.1
$ fbrnch --help
Fedora branch building tool

Usage: fbrnch [--version] COMMAND
  A tool to help with updating and building package branches
  https://github.com/juhp/fbrnch#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  clone                    clone packages
  switch                   Switch branch
  nvr                      Print name-version-release
  status                   Status package/branch status
  merge                    Merge from newer branch
  build                    Build package(s) in Koji
  list                     List packages in pagure
  branches                 List package branches
  parallel                 Parallel build packages in Koji
  sidetags                 List user's side-tags
  override                 Tag builds into buildroot override in Koji
  waitrepo                 Wait for build to appear in Koji buildroot
  scratch                  Scratch build package in Koji
  update                   Update package in dist-git to newer version
  sort                     Sort packages in build dependency order
  prep                     Prep sources
  local                    Build locally
  srpm                     Build srpm
  diff                     Diff local changes
  compare                  Show commits between branches
  mock                     Local mock build
  install-deps             Install package build dependencies
  install                  Build locally and install package(s)
  not-installed            Packages not installed locally
  bugs                     List package bugs
  bump                     Bump release for package
  commit                   Git commit packages
  pull                     Git pull packages
  push                     Git push packages
  create-review            Create a Package Review request
  update-review            Update a Package Review
  review-package           Run fedora-review on a package Review Request bug
  reviews                  List package reviews
  request-repos            Request dist git repo for new approved packages
  import                   Import new approved created packages from bugzilla
                           review
  request-branches         Request branches for approved created packages
  find-review              Find package review bug
  command                  Run shell command in package dirs ($p)
  copr                     Build package(s) in Fedora Copr
  rename-rawhide           Rename local 'master' branch to 'rawhide'
  count                    Count number of living packages
  graph                    Output dependency graph
  ftbfs                    Check FTBFS status
```

## Installation
fbrnch is packaged in Fedora: `sudo dnf install fbrnch`.

## Build from source
1. Install openssl-devel

2. Clone the git repo.

3. Then either:

a) using stack >= 2.1: `stack install`

or

b) with cabal-install (probably 2.4 or later) and cabal-rpm:

```
$ cabal-rpm builddep
$ cabal new-install --installdir=~/bin
```

## Required runtime tools
fbrnch currently uses these fedora cli tools:
- fedpkg
- bodhi
- koji
- copr-cli
for pushing packages.

It also makes use of:
- curl
- rpmbuild & rpmspec
- klist and fkinit
- git
- ssh & scp (for uploading package reviews)
- bugzilla API key

## rpmbuild configuration
You may want to set `~/.rpmmacros` to use particular directories,
since unlike fedpkg, fbrnch follows the macros set in `~/.rpmmacros`.

By default rpmbuild uses `~/rpmbuild/`.

Two common alternative configurations might be either:

1) use the package directory for everything (like fedpkg does):
```
%__pwd %(echo $PWD)
%_builddir %__pwd
%_rpmdir %__pwd
%_sourcedir %__pwd
%_specdir %__pwd
%_srcrpmdir %__pwd
```
These are easy to find, but do create a lot of clutter.

2) Lately I just set _topdir to pwd
```
%__pwd %(echo $PWD)
%_topdir %__pwd
%_sourcedir %__pwd
%_specdir %__pwd
```
With this rpmbuild creates a bunch of dirs in each package dir
(like the ones in ~/rpmbuild/), which can hide srpms and build trees, etc.

## Bugzilla API key
fbrnch can share the API of the python-bugzilla CLI tool,
placed either in `~/.config/python-bugzilla/bugzillarc` or `~/.bugzillarc`:

```
[bugzilla.redhat.com]
api_key = PASTE_YOUR_APIKEY_HERE

```

You can create your key at
<https://bugzilla.redhat.com/userprefs.cgi?tab=apikey>.

## Known issues
- parallel builds will push local package commits without asking
- currently it only checks if already built by NVR not githash
- authentication is not implemented yet natively for Koji, Bodhi, Pagure
  (and source upload)
  - so python clients are used for "writing"
    (specifically koji, bodhi-client, fedpkg),
    but all queries are done directly by Web APIs for speed and control.
- https checkouts are currently treated as anonymous git checkouts

## Motivation, history, talks
This project started off (as "fedbrnch") basically as a simple tool to
build a package across branches (ie for current releases).  Then bugzilla
and Bodhi integration was added, and gradually more features, including
some generic commands across packages which had already been done before
in fedora-haskell-tools.

I have given a couple of short talks about fbranch:
- Nest with Fedora: [youtube](https://www.youtube.com/watch?v=40kTBsA674U) and [slides](https://github.com/juhp/presentations/blob/master/fedora-nest-2020-fbrnch/fbrnch-nest.md)
- Lightning talk at devconf.cz 2021: [youtube](https://www.youtube.com/watch?v=O2-6rDuPMRA&t=2s)

## Contribute
Bug reports, feedback, and pull requests are all highly appreciated.

Please report any unsupported or unintuitive workflow steps.

See the TODO list and also the FIXME comments scattered across the source.
Do open an issue before embarking on larger changes.

Authors of the code:

<a href="https://github.com/juhp/fbrnch/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=juhp/fbrnch" />
</a>
