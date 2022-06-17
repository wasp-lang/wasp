module Wasp.SemanticVersion
  ( Version (..),
    Operator (..),
    Comparator (..),
    ComparatorSet (..),
    Range (..),
    isVersionInRange,
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

data Operator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of operator.
instance Show Operator where
  show Equal = "="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

data Comparator = Comparator Operator Version
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show Comparator where
  show (Comparator op v) = show op ++ show v

newtype PrimitiveComparatorSet = PrimitiveComparatorSet (NonEmpty Comparator)
  deriving (Eq)

data SpecialComparatorSet = Caret Version
  deriving (Eq)

data ComparatorSet
  = CSPrimitive PrimitiveComparatorSet
  | CSSpecial SpecialComparatorSet
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show PrimitiveComparatorSet where
  show (PrimitiveComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show SpecialComparatorSet where
  show (Caret version) = "^" ++ show version

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (CSPrimitive cs) = show cs
  show (CSSpecial cs) = show cs

data Range = Range [ComparatorSet]
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of version range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0

doesVersionSatisfyComparator :: Version -> Comparator -> Bool
doesVersionSatisfyComparator version (Comparator operator compVersion) = case operator of
  Equal -> version == compVersion
  LessThan -> version < compVersion
  LessThanOrEqual -> version <= compVersion
  GreaterThan -> version > compVersion
  GreaterThanOrEqual -> version >= compVersion

doesVersionSatisfyPrimitiveComparatorSet :: Version -> PrimitiveComparatorSet -> Bool
doesVersionSatisfyPrimitiveComparatorSet version (PrimitiveComparatorSet comps) = all (doesVersionSatisfyComparator version) comps

specialComparatorSetToPrimitiveComparatorSet :: SpecialComparatorSet -> PrimitiveComparatorSet
specialComparatorSetToPrimitiveComparatorSet (Caret version) =
  PrimitiveComparatorSet $
    NE.fromList
      [ Comparator GreaterThanOrEqual version,
        Comparator LessThan (nextBreakingChangeVersion version)
      ]

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version comparatorSet =
  let primitiveComparatorSet = case comparatorSet of
        CSPrimitive cs -> cs
        CSSpecial cs -> specialComparatorSetToPrimitiveComparatorSet cs
   in doesVersionSatisfyPrimitiveComparatorSet version primitiveComparatorSet

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

-- TODO: This is a problem now, I can't put multiple ComparatorSet into one range while keeping them intersected since I assume they are OR and not AND.
rangeFromVersionsIntersection :: [(Operator, Version)] -> Range
rangeFromVersionsIntersection [] = Range []
rangeFromVersionsIntersection compPairs = Range $ pure $ ComparatorSet $ uncurry Comparator <$> NE.fromList compPairs

-- Helper methods for constructing ranges.

lt :: Version -> Range
lt = rangeFromPrimitiveComparatorSet LessThan

lte :: Version -> Range
lte = rangeFromPrimitiveComparatorSet LessThanOrEqual

gt :: Version -> Range
gt = rangeFromPrimitiveComparatorSet GreaterThan

gte :: Version -> Range
gte = rangeFromPrimitiveComparatorSet GreaterThanOrEqual

eq :: Version -> Range
eq = rangeFromPrimitiveComparatorSet Equal

caret :: Version -> Range
caret = rangeFromSpecialComparatorSet . Caret

rangeFromPrimitiveComparatorSet :: Operator -> Version -> Range
rangeFromPrimitiveComparatorSet op = Range . pure . CSPrimitive . PrimitiveComparatorSet . pure . Comparator op

rangeFromSpecialComparatorSet :: SpecialComparatorSet -> Range
rangeFromSpecialComparatorSet = Range . pure . CSSpecial

-- data Comparator = ComparatorP PrimitiveComparator | ComparatorS SpecialComparator
--   deriving (Eq)

-- newtype PrimitiveComparator = PrimitiveComparator Operator Version
--   deriving (Eq)

-- data SpecialComparator = Caret Version
--   deriving (Eq)

-- data ComparatorSet = ComparatorSet (NonEmpty Comparator)
--   deriving (Eq)

-- data PrimitiveComparatorSet = PrimitiveComparatorSet (NonEmpty PrimitiveComparator)
--   deriving (Eq)
