module Types (
  Archs(..),
  SideTagTarget(..),
  maybeTarget
  )
where

data Archs = Archs [String] | ExcludedArchs [String]

data SideTagTarget = SideTag | Target String
  deriving Eq

maybeTarget :: Maybe SideTagTarget -> Maybe String
maybeTarget (Just (Target t)) = Just t
maybeTarget _ = Nothing
