# fbrnch - "Fed Brunch"

[![GPL-2+ license](https://img.shields.io/badge/license-GPL--2+-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/fbrnch/workflows/build/badge.svg)](https://github.com/juhp/fbrnch/actions)
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)

A tool to help Fedora Packagers build package branches and add new packages.

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
- creating, updating and listing package reviews
- requesting new repos and branches
- importing new packages
- progressive copr builds
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
$ fbrnch switch f34 [package] ...
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
$ fbrnch merge f33 package
```
which will offer to merge f34 (or some of it) into f33.

Merging can also be done together with building:
```
$ fbrnch build f34 package
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

See `fbrnch --help` for details and the full list.

## Known issues
- parallel builds will push local package commits without asking
- currently it only checks if already built by NVR not githash
- authentication is not implemented yet natively for Koji, Bodhi, Pagure
  (and source upload)
  - so python clients are used for "writing"
    (specifically koji, bodhi-client, fedpkg),
    but all queries are done directly by Web APIs for speed and control.

## Motivation, history, talks
This project started off (as "fedbrnch") basically as a simple tool to
build a package across branches (ie for current releases).  Then bugzilla
and Bodhi integration was added, and gradually more features, including
some generic commands across packages which had already been done before
in fedora-haskell-tools.

I have given a couple of short talks about fbranch:
- Nest with Fedora: [youtube](https://www.youtube.com/watch?v=40kTBsA674U) and [slides](https://github.com/juhp/presentations/blob/master/fedora-nest-2020-fbrnch/fbrnch-nest.md)
- Lightning talk at devconf.cz 2021: [youtube](https://www.youtube.com/watch?v=O2-6rDuPMRA&t=2s)

## Installation
fbrnch is packaged in Fedora: `sudo dnf install fbrnch`.

## Build from source
1. Install openssl-devel

2. Clone the git repo and in the source dir, setup the git submodules:

`git submodule update --init`

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

## Contribute
Bug reports, feedback, and pull requests welcome.

Do report any unsupported or unintuitive workflow steps.

See the TODO list and also the FIXME comments scattered across the source.
Please open an issue before embarking on large changes.

Committers so far:

<a href="https://github.com/juhp/fbrnch/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=juhp/fbrnch" />
</a>
