module Wasp.SemanticVersion
  ( Version (..),
    Range (..),
    ComparatorSet,
    isVersionInRange,
    lt,
    lte,
    gt,
    gte,
    eq,
    backwardsCompatibleWith,
  )
where

import Data.List (intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Numeric.Natural
import Text.Printf (printf)

-- Implements SemVer (semantic versioning) by following spec from https://github.com/npm/node-semver .

data Version = Version
  { major :: Natural,
    minor :: Natural,
    patch :: Natural
  }
  deriving (Eq, Ord)

-- | We rely on this `show` implementation to produce valid semver representation of version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data Operator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of version.
instance Show Operator where
  show Equal = "="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

data Comparator
  = PrimitiveComparator Operator Version
  | BackwardsCompatibleWith Version
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of comparator.
instance Show Comparator where
  show (PrimitiveComparator op v) = show op ++ show v
  show (BackwardsCompatibleWith v) = "^" ++ show v

data ComparatorSet = ComparatorSet (NE.NonEmpty Comparator)
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two comparator sets as a union of their comparators.
instance Semigroup ComparatorSet where
  (ComparatorSet compsl) <> (ComparatorSet compsr) = ComparatorSet $ NE.nub $ compsl <> compsr

data Range = Range [ComparatorSet]
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of version range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as a union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ nub $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version (ComparatorSet comps) =
  all (doesVersionSatisfyComparator version) comps

doesVersionSatisfyComparator :: Version -> Comparator -> Bool
doesVersionSatisfyComparator version (BackwardsCompatibleWith refVersion) =
  all
    (doesVersionSatisfyComparator version)
    [ PrimitiveComparator GreaterThanOrEqual refVersion,
      PrimitiveComparator LessThan (nextBreakingChangeVersion refVersion)
    ]
doesVersionSatisfyComparator version (PrimitiveComparator operator compVersion) =
  case operator of
    Equal -> version == compVersion
    LessThan -> version < compVersion
    LessThanOrEqual -> version <= compVersion
    GreaterThan -> version > compVersion
    GreaterThanOrEqual -> version >= compVersion

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0

-- Helper methods.

lt :: Version -> ComparatorSet
lt = mkPrimCompSet LessThan

lte :: Version -> ComparatorSet
lte = mkPrimCompSet LessThanOrEqual

gt :: Version -> ComparatorSet
gt = mkPrimCompSet GreaterThan

gte :: Version -> ComparatorSet
gte = mkPrimCompSet GreaterThanOrEqual

eq :: Version -> ComparatorSet
eq = mkPrimCompSet Equal

backwardsCompatibleWith :: Version -> ComparatorSet
backwardsCompatibleWith = ComparatorSet . pure . BackwardsCompatibleWith

mkPrimCompSet :: Operator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . PrimitiveComparator op
