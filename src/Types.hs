module Types (
  Archs(..)
  )
where

data Archs = Archs [String] | ExcludedArchs [String]
