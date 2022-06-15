module Wasp.SemanticVersion
  ( Version (..),
    Operator (..),
    Comparator (..),
    ComparatorSet (..),
    Range (..),
    isVersionInRange,
    rangeFromVersion,
    rangeFromVersionsIntersection,
  )
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Numeric.Natural
import Text.Printf (printf)

-- Implements semantic version by following spec from https://github.com/npm/node-semver .

data Version = Version
  { major :: Natural,
    minor :: Natural,
    patch :: Natural
  }
  deriving (Eq, Ord)

instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data Operator = Equal | LessThanOrEqual | BackwardsCompatibleWith

instance Show Operator where
  show Equal = "="
  show LessThanOrEqual = "<="
  show BackwardsCompatibleWith = "^"

data Comparator = Comparator Operator Version

instance Show Comparator where
  show (Comparator op v) = show op ++ show v

data ComparatorSet = ComparatorSet (NonEmpty Comparator)

instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> toList comps

data Range = Range (NonEmpty ComparatorSet)

instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> toList compSets

instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ csets1 <> csets2

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0

doesVersionSatisfyComparator :: Version -> Comparator -> Bool
doesVersionSatisfyComparator version (Comparator operator compVersion) = case operator of
  Equal -> version == compVersion
  LessThanOrEqual -> version <= compVersion
  BackwardsCompatibleWith -> version >= compVersion && version < nextBreakingChangeVersion compVersion

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version (ComparatorSet comps) = all (doesVersionSatisfyComparator version) comps

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

rangeFromVersion :: (Operator, Version) -> Range
rangeFromVersion = Range . pure . ComparatorSet . pure . uncurry Comparator

rangeFromVersionsIntersection :: NonEmpty (Operator, Version) -> Range
rangeFromVersionsIntersection = Range . pure . ComparatorSet . (uncurry Comparator <$>)
