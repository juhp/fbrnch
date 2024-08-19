module Types (
  Archs(..),
  SideTagTarget(..),
  maybeTarget,
  ChangeType(..)
  )
where

data Archs = Archs [String] | ExcludedArchs [String]

-- FIXME: --new-sidetag ?
data SideTagTarget = SideTag | Target String
  deriving Eq

maybeTarget :: Maybe SideTagTarget -> Maybe String
maybeTarget (Just (Target t)) = Just t
maybeTarget _ = Nothing

data ChangeType = ChangeBodhi | ChangeCommit | ChangeReview
  deriving Eq
