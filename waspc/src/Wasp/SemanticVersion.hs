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
import Data.List.NonEmpty (NonEmpty)
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

-- | We rely on `show` here to produce valid semver representation of version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data Operator = Equal | LessThanOrEqual | BackwardsCompatibleWith
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of operator.
instance Show Operator where
  show Equal = "="
  show LessThanOrEqual = "<="
  show BackwardsCompatibleWith = "^"

data Comparator = Comparator Operator Version
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show Comparator where
  show (Comparator op v) = show op ++ show v

data ComparatorSet = ComparatorSet (NonEmpty Comparator)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

data Range = Range (NonEmpty ComparatorSet)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of version range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> NE.toList compSets

-- | We define concatenation of two version ranges as union of their comparator sets.
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

data Bound = Included {boundVersion :: Version} | Excluded {boundVersion :: Version}
  deriving (Eq)

data BoundsRange = LeftToRight Bound Bound | LeftToInf Bound

mkBoundsRange :: Bound -> Maybe Bound -> BoundsRange
mkBoundsRange bl Nothing = LeftToInf bl
mkBoundsRange bl (Just br) | boundVersion bl <= boundVersion br = LeftToRight bl br
mkBoundsRange bl@(Included vl) (Just br@(Included vr)) | vl == vr = LeftToRight bl br
mkBoundsRange bl br = error "Invalid bounds range: " <> show bl <> show br

rangeToBounds :: Range -> [BoundsRange]
rangeToBounds = error "TODO"

comparatorToBounds :: Comparator -> [BoundsRange]
comparatorToBounds (Comparator op version) = error "TODO"
