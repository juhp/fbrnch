module Types (
  BranchesRequest(..),
  Package,
  Scratch(..)
  ) where

import Distribution.Fedora.Branch

data BranchesRequest = AllReleases | BranchesRequest [Branch]

type Package = String

data Scratch = AllArches | Arch String
