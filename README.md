# fbrnch - "Fed Brunch"

[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)
<!---
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)
[![Stackage Lts](http://stackage.org/package/fbrnch/badge/lts)](http://stackage.org/lts/package/fbrnch)
[![Stackage Nightly](http://stackage.org/package/fbrnch/badge/nightly)](http://stackage.org/nightly/package/fbrnch)
-->
Tool to help Fedora Packagers build package branches and add new packages.

Fedora developers use a lot of time building packages across releases
and workflow for adding new packages, etc. The motivation for fbrnch is
to help to (semi-)automate common workflows to save time and effort.

## Description
`fbrnch` is a Fedora Packager client which tries to automate some common tasks
like:

- merging and building a package across release branches
- creating package reviews
- listing approved package reviews
- requesting repo and branches
- importing new packages
- automatic parallel builds of sets of packages in dependency order

## Usage

### Creating new packages/branches

#### Creating a new package
```
$ fbrnch create-review my-new-package.spec
```
This will create (or update) an srpm, run rpmlint,
then upload it to fedorapeople, perform a scratch build,
and open a Review Request in Bugzilla (similar to fedora-create-review).

#### Update a package review
```
$ fbrnch update-review my-new-package.spec
```
Similar to create-review: uploads to fedorapeople and posts
updated package links to the open package review.

#### List open package reviews
```
$ fbrnch reviews --help
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
$ fbrnch switch -b master [package] ...
```

You can also git pull:
```
$ fbrnch pull [package] ...
```
#### Package status
```
$ fbrnch status [package]
```
outputs information about the status of each branch.
The status command can also be used with `--reviews`
to check the build status of new packages.

List package bugs:
```
$ fbrnch bugs [package]
```

#### Merging and Building in Koji
You can merge branches with:
```
$ fbrnch merge -b f31 package
```
which will offer to merge f32 (or some of it) into f31.

Merging can also be done together with building:
```
$ fbrnch build package
```
will offer to merge newer commits from the newer branch.
Otherwise if a branch NVR is also ready pushed and built it will be skipped.
Branch builds are pushed to Bodhi.

You can of course specify which branch(es) to build:
```
$ fbrnch build -b f32 package
```

Scratch builds can also be done:
```
$ fbrnch scratch -b master
```

You can sort packages by build dependency order:
```
$ fbrnch sort -b master package1 package2 package3 package4 ...
```

### Local commands
```
$ fbrnch prep -b master package
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

## Known issues
- if you don't specify branches, fbrnch build will try to do all pushed branches
- only checks already built by NVR not githash
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
On Fedora you can use my [copr repo](https://copr.fedorainfracloud.org/coprs/petersen/fbrnch/).

Run `stack install` or `cabal new-install` in a git checkout.

## Contributing
Bug reports, feedback, and fixes are welcome.

See the TODO list and also scattered source FIXME comments.
As usual better to ask before embarking on large changes.

## Usual disclaimer
This is still in active development.
While it is generally works well for me,
if it should breaks things for you, you get to keep the pieces. :)
But please report unsupported or unintuitive workflows.
