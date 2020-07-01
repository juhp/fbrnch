# fbrnch - "Fed Brunch"

[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)
<!---
[![Hackage](https://img.shields.io/hackage/v/fbrnch.svg)](https://hackage.haskell.org/package/fbrnch)
[![Stackage Lts](http://stackage.org/package/fbrnch/badge/lts)](http://stackage.org/lts/package/fbrnch)
[![Stackage Nightly](http://stackage.org/package/fbrnch/badge/nightly)](http://stackage.org/nightly/package/fbrnch)
-->
Tool to help Fedora Packagers build package branches

## Description

fbrnch is a Fedora Packager client which tries to automate some common tasks
like:

- merging and building a package across release branches
- creating package reviews
- listing approved package reviews
- requesting repo and branches
- importing new packages

## Usage
```
$ fbrnch --help
Fedora package branch building tool

Usage: fbrnch [--version] COMMAND
  This tool helps with updating and building package branches

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  clone                    clone packages
  switch                   Switch branch
  status                   Status package/branch status
  merge                    Merge from newer branch
  build                    Build package(s)
  sort                     Sort packages in build dependency order
  prep                     Prep sources
  local                    Build locally
  install                  Build locally and install package(s)
  bugs                     List package bugs
  pull                     Git pull packages
  create-review            Create a Package Review request
  update-review            Update a Package Review
  reviews                  List package reviews
  request-repos            Request dist git repo for new approved packages
  import                   Import new approved created packages from bugzilla
                           review
  request-branches         Request branches for approved created packages
  find-review              Find package review bug
  test-bz-token            Check bugzilla login status
```

## Known issues

- currently prep doesn't pull down source (will fixed soon)
- no support yet for overrides (coming)

## Installation

Run `stack install` or `cabal new-install` in a git checkout.

## Contributing

This is still in development: feedback and contributions are welcome.

## Usual disclaimer
This is still in active development.
While it is generally works well for me,
if it breaks things for you, you get to keep the pieces. :)
Bug reports are welcome.
