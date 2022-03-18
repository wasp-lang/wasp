module Wasp.SemanticVersion
  ( SemanticVersion (..),
    VersionBounds (..),
    isVersionInBounds,
  )
where

import Text.Printf (printf)

data SemanticVersion = SemanticVersion
  { major :: Int,
    minor :: Int,
    patch :: Int
  }
  deriving (Eq, Ord)

instance Show SemanticVersion where
  show (SemanticVersion mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data VersionBounds
  = Exact SemanticVersion
  | CompatibleWith SemanticVersion

instance Show VersionBounds where
  show (CompatibleWith version) = "^" ++ show version
  show (Exact version) = show version

isVersionInBounds :: SemanticVersion -> VersionBounds -> Bool
isVersionInBounds version bounds = case bounds of
  (CompatibleWith reference) -> major version == major reference
  (Exact reference) -> version == reference
