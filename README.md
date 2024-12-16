# fbrnch (fedora branch) packager tool

[![GPL-2+ license](https://img.shields.io/badge/license-GPL--2+-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/fbrnch/workflows/build/badge.svg)](https://github.com/juhp/fbrnch/actions)
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)

A tool to help Fedora Packagers build package branches.

Fedora developers use a lot of time building packages across releases
and workflow for adding new packages, etc.
fbrnch was made to help (semi-)automate common workflows to save time
and effort, and avoid some common mistakes.

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

### Cloning and switching branch
Clone one or more packages:
```
$ fbrnch clone [package] ...
```
There are also options to clone all one's packages or another user's packages.

One can change the branch of one or more packages:
```
$ fbrnch switch f41 [package] ...
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
The update-sources and update-version commands can help with updating
a package after editing the spec file to a newer version:
```
$ fbrnch update-sources
```
which will download the new tarball and upload it, etc.

The `bump` command can be used to increase the release field for packages.

You can commit to the current branch:
```
$ fbrnch commit
```
It uses any rpm changelog for the commit message,
or you can pass `-m "..."` or amend with `-a`.

You can merge branches with:
```
$ fbrnch merge f40 [package]
```
which will offer to merge f41 (or up to a git hash you choose) into f40.

Merging can also be done together with building:
```
$ fbrnch build f41 [package]
```
will ask if you want to merge newer commits from a newer branch,
then push and build it.

If the branch is also already pushed and NVR built it will be skipped.
If the NVR is currently building it will be picked up by `fbrnch build`.
Completed branch builds can be pushed to Bodhi.

You can also build all active branches:
```
$ fbrnch build -B [package]
```
or only all fedora branches:
```
$ fbrnch build -F [package]
```
or only all epel branches:
```
$ fbrnch build -E [package]
```

Scratch builds can also be done:
```
$ fbrnch scratch rawhide
```
optionally a different koji `--target` can be given.

There are arch short-cut aliases: `scratch-x86_64` and `scratch-aarch64`.

You can sort packages by build dependency order:
```
$ fbrnch sort rawhide <package1> <package2> <package3> <package4> ...
```

### Local commands
```
$ fbrnch prep rawhide [package]
```

Build locally:
```
$ fbrnch local
```
this works in the current package dir like other commands,
installing any missing dependencies with `sudo dnf builddep`.
(It also works for a non-git package dir.)

Or one can specify the path to the package.

Locally build and install:
```
$ fbrnch install <package1> <package2>/ <package3> ...
```

You can use:
```
$ fbrnch install-deps [package]
```
to only install the dependencies of a package.

Use
```
$ fbrnch rename-rawhide [package]
```
to rename an old local master branch to rawhide.

### Parallel building
fbrnch can sort packages automatically and build them in parallel
in Koji in dependency layers (using low-priority background builds
to avoid grabbing too many Koji resources).

```
$ fbrnch parallel --sidetag rawhide pkg-x pkg-y pkg-z pkg-xy pkg-xy-z
```
builds a list of packages in a sidetag (generating it if no sidetags exist)
in parallel ordered by build dependencies.

For a long list of packages, it is generally better to
`fbrnch sort` them first and then use the generated chain-build style
output as arguments to `fbrnch parallel`.

When building for a branch, merging from the next newer branch will be offered
unless using `--no-merge`
(then you may prefer to run `fbrnch merge <branch> ...` first instead).

Except for rawhide using a --sidetag or --target is required.
If you have more than one active sidetag for a branch,
you can select one using `--target`.
They can be listed with `fbrnch sidetags`.

After parallel building completes,
you can create a Bodhi update from the sidetag.

It is also possible to build a package in parallel across branches
and push them to Bodhi.

### Creating new packages/branches

#### Creating a new package
```
$ fbrnch create-review [my-new-package]
```
This will create an srpm (or update it if the spec file is newer), run rpmlint,
optionally perform a scratch build, then upload the spec and srpm to
fedorapeople, and open a Review Request in Bugzilla
(similar to fedora-create-review).

#### Update a package review
```
$ fbrnch update-review [my-new-package]
```
Similar to create-review: it uploads the updated files to fedorapeople
and posts the updated package urls to the open package review
with an optional scratch build.

#### List open package reviews
To list one's open package reviews:
```
$ fbrnch reviews
```
They can be filtered by status with various options like
`--approved` or `--created` (or another bugzilla `--submitter`), etc.

One can also search for the review(s) of a specific package with:
```
$ fbrnch find-review <package-name>
```

Package reviews can be performed with `fbrnch review-package`.
It also has a lightweight `--interactive` mode for streamlined reviews.

#### Request repos
Once a review has been approved
```
$ fbrnch request-repos
```
will request repo(s) for approved package(s) and offer to request branches.

#### Import a new package
After the repo has been created
```
$ fbrnch import
```
will clone the repo(s) and offer to import the srpm
directly from the latest url in the approved package review(s),
which can then be built directly into Koji Rawhide
and the package review(s) updated.

#### Request branches
If you prefer you can request branches after the repo is created
(or package is imported) with
```
$ fbrnch request-branches
```
which will be prompt for which branches you want, unless already given.

Optionally a mock build per branch can be done first.


### Other commands
There are a lot more commands, like eg `copr` and `graph`:

## Help
`$ fbrnch --version`

```
1.6.1
```

`$ fbrnch --help`

```
Fedora branch building tool

Usage: fbrnch [--version] COMMAND

  A tool to help with updating and building package branches
  https://github.com/juhp/fbrnch#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  clone                    Clone packages
  switch                   Switch branch
  nvr                      Print name-version-release
  status                   Status package/branch status
  merge                    Merge from newer branch
  unpushed                 Show unpushed commits
  build                    Build package(s) in Koji
  list                     List packages in pagure
  list-local               List packages in branch
  branches                 List package branches
  parallel                 Parallel build packages in Koji
  sidetags                 List user's side-tags
  override                 Tag builds into buildroot override in Koji
  waitrepo                 Wait for build to appear in Koji buildroot
  scratch                  Scratch build package in Koji
  scratch-aarch64          Koji aarch64 scratch build of package
  scratch-x86_64           Koji x86_64 scratch build of package
  update-sources           Download and update newer sources
  update-version           Update package in dist-git to newer version
  sort                     Sort packages in build dependency order (default
                           format: chain-build)
  prep                     Prep sources
  local                    Build locally
  srpm                     Build srpm
  srpm-spec                Show the spec file in an srpm
  diff                     Diff local changes
  compare                  Show commits between branches
  src-deps                 List source package dependencies
  mock                     Local mock build
  builddeps                Install package build dependencies
  install                  Build locally and install package(s)
  not-installed            Packages not installed locally
  bugs                     List package bugs
  bump                     Bump release for package
  commit                   Git commit packages
  pull                     Git pull packages
  fetch                    Git fetch packages
  push                     Git push packages
  owner                    List package owner(s)
  bzusers                  Search bugzilla users
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
  autospec                 Convert package to use rpmautospec
  move-artifacts           Move old rpm artifacts into rpmbuild dirs
```

Use `fbrnch <cmd> --help` to get specific help about each of the above commands
and their options.

## Installation
fbrnch is packaged in Fedora: `sudo dnf install fbrnch`.

## Build from source
1. Install openssl-devel

2. Clone the git repo.

3. Then either:

a) using stack: `stack install`

or

b) with cabal-install and cabal-rpm:

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
since unlike fedpkg, fbrnch follows the local rpmbuild directory macros:
rpmbuild defaults to directories under `~/rpmbuild/`
but this can be overridden by the user in `~/.rpmmacros` as follows.

Two common alternative configurations in `~/.rpmmacros` might be either:

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
- currently it only checks if already built by NVR not githash
- parallel and sort, etc do not take pkgconfig() and other meta() deps into
  account yet (this should be fixed soon in rpmbuild-order)

## To do
- authentication is not implemented yet natively for Koji, Bodhi, Pagure
  (and source upload)
  - so python clients are used for "writing"
    (specifically koji, bodhi-client, fedpkg),
    but all queries are done directly by Web APIs for speed and control.

## Motivation, history, talks
This project started off as a simple tool to build a package across branches
(ie for current releases).  Then bugzilla and Bodhi integration was added,
and gradually more features, including some generic commands across packages
inspired by the older fedora-haskell-tools project.

I have given a couple of short talks about fbrnch:
- Nest with Fedora 2020: [youtube](https://www.youtube.com/watch?v=40kTBsA674U) and [slides](https://github.com/juhp/presentations/blob/master/fedora-nest-2020-fbrnch/fbrnch-nest.md)
- Lightning talk for devconf.cz 2021: [youtube](https://www.youtube.com/watch?v=O2-6rDuPMRA&t=2s)

## Contribute
Bug reports, feedback, and pull requests are all much appreciated.

Do report any unsupported or inconsistent workflow steps.

See the TODO list and also the FIXME comments scattered across the source.

Authors of the code:

<a href="https://github.com/juhp/fbrnch/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=juhp/fbrnch" />
</a>
