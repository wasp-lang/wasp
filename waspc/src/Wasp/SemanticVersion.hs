module Wasp.SemanticVersion
  ( Version (..),
    VersionBounds (..),
    isVersionInBounds,
  )
where

import Text.Printf (printf)

data Version = Version
  { major :: Int,
    minor :: Int,
    patch :: Int
  }
  deriving (Eq, Ord)

instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data VersionBounds
  = -- | Allows only the version exactly equal to the one specified in the bounds
    Exact Version
  | -- | Allows changes that do not modify the leftmost non-zero digit in major.minor.patch, as described
    -- in node semver docs: https://github.com/npm/node-semver#caret-ranges-123-025-004
    BackwardsCompatibleWith Version

instance Show VersionBounds where
  show (BackwardsCompatibleWith version) = "^" ++ show version
  show (Exact version) = show version

isVersionInBounds :: Version -> VersionBounds -> Bool
isVersionInBounds version bounds = case bounds of
  (BackwardsCompatibleWith reference) -> version >= reference && version < nextBreakingChangeVersion reference
  (Exact reference) -> version == reference

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0