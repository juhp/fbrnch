# fbrnch - "Fed Brunch"

[![GPL-2+ license](https://img.shields.io/badge/license-GPL--2+-blue.svg)](LICENSE)
[![GitHub CI](https://github.com/juhp/fbrnch/workflows/Haskell/badge.svg)](https://github.com/juhp/fbrnch/actions)
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)

A tool to help Fedora Packagers build package branches and add new packages.

Fedora developers use a lot of time building packages across releases
and workflow for adding new packages, etc. The motivation for fbrnch is
to help to (semi-)automate common workflows to save time and effort.

fbrnch is distributed under the GPL license version 2 or later.

## Description
`fbrnch` is a Fedora Packager client which tries to automate some common tasks
like:

- merging and building a package across release branches
- automatic parallel builds of sets of packages in dependency order
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
Similar to create-review: uploads to fedorapeople and posts
updated package links to the open package review.

#### List open package reviews
```
$ fbrnch reviews
```
This lists one's open package reviews.
Various options like `--approved` or `--created` allow filtering by status.

One can also search for reviews with:
```
$ fbrnch find-review package-name
```

#### Request repos
Once a review has been approved
```
$ fbrnch request-repos
```
will request repos for approved package(s).

#### Import a new package
With fbrnch this can be done in one step - no need to clone first.
```
$ fbrnch import [my-new-package]
```
will offer to import the srpm from the approved review
(similar to `fedpkg import`).
Without any arguments it will offer to import any approved package reviews
one by one.
The imported package is then built in Koji Rawhide,
and the package review updated.

#### Request branches
Finally you can request branches with
```
$ fbrnch request-branches
```
which will confirm which branches you want, unless given.

Optionally a mock build can be done first.


### Building
#### Cloning and switching branch
```
$ fbrnch clone [package] ...
```
(one can also clone all one's packages or another user's packages).

```
$ fbrnch switch f33 [package] ...
```

You can also git pull:
```
$ fbrnch pull [package] ...
```
#### Package status
```
$ fbrnch status -B [package]
```
outputs information about the status of each branch.
The status command can also be used with `--reviews`
to check the build status of new packages.

List package bugs:
```
$ fbrnch bugs [package]
```

#### Commit, Merging and Building in Koji
You can commit to the current branch:
```
$ fbrnch commit
```
uses any rpm changelog, or you can pass `-m "..."` or amend with `-a`.

You can merge branches with:
```
$ fbrnch merge f32 package
```
which will offer to merge f33 (or some of it) into f32.

Merging can also be done together with building:
```
$ fbrnch build f33 package
```
will offer to merge newer commits from the newer branch.
If the branch NVR is also ready pushed and built it will be skipped.
Branch builds are pushed to Bodhi.

You can also build all branches:
```
$ fbrnch build -B package
```

Scratch builds can also be done:
```
$ fbrnch scratch rawhide
```
optionally a different koji target can be given.

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
this works in the current package dir like other commands
and one can also specify package.

Locally build and install:
```
$ fbrnch install package1 package2 package3 ...
```

### Parallel building
fbrnch can automatically sort packages and build them in parallel
in Koji in dependency layers (using low-priority background builds
to avoid grabbing too many Koji resources).

```
$ fbrnch parallel --sidetag rawhide pkg-x pkg-y pkg-z pkg-xy pkg-xyz
```
builds a list of packages in parallel ordered by build dependencies.

Note it requires a sidetag in general, so you need to manage and select them
if using more than one per branch with `--target`.

### Other commands
There are more commands like `nvr`, `install-deps`, `copr`.

See `fbrnch --help` for details and the full list.

## Known issues
- parallel builds will push local package commits without asking
- currently only checks if already built by NVR not githash
- authentication is not implemented yet natively for Koji, Bodhi, Pagure
  (and source upload)
  - so python clients are used for "writing"
    (specifically koji, bodhi-client, fedpkg),
    but all queries are done directly by Web RPC for speed and control.

## Motivation and history
This project started off (as "fedbrnch") basically as a simple tool to
build a package across branches (ie for current releases).  Then bugzilla
and Bodhi integration was added, and gradually more features, including
some generic commands across packages which had already been done before
in fedora-haskell-tools.

## Installation
On Fedora the easiest way to install is using my [copr repo](https://copr.fedorainfracloud.org/coprs/petersen/fbrnch/).

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

## Contributing
Bug reports, feedback, and fixes are welcome.

See the TODO list and also the many FIXME comments scattered across the source.
It is better to ask before embarking on large changes.

## Usual disclaimer
This is still in active development.
While it is generally works well for me,
if it should breaks things for you, you get to keep the pieces. :)
But please do report unsupported or unintuitive workflows.
