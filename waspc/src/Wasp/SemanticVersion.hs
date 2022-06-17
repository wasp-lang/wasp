module Wasp.SemanticVersion
  ( Version (..),
    Range (..),
    isVersionInRange,
    lt,
    lte,
    gt,
    gte,
    eq,
    caret,
    (&),
  )
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, fromList)
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

data PrimitiveComparator = PrimitiveComparator Operator Version
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show PrimitiveComparator where
  show (PrimitiveComparator op v) = show op ++ show v

data SpecialComparator = Caret Version
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show SpecialComparator where
  show (Caret v) = "^" ++ show v

data Comparator = ComparatorP PrimitiveComparator | ComparatorS SpecialComparator
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show Comparator where
  show (ComparatorP c) = show c
  show (ComparatorS c) = show c

data ComparatorSet = ComparatorSet (NonEmpty Comparator)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two comparator sets union union of their comparators.
instance Semigroup ComparatorSet where
  (ComparatorSet compsl) <> (ComparatorSet compsr) = ComparatorSet $ compsl <> compsr

data PrimitiveComparatorSet = PrimitiveComparatorSet (NonEmpty PrimitiveComparator)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show PrimitiveComparatorSet where
  show (PrimitiveComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two comparator sets union union of their comparators.
instance Semigroup PrimitiveComparatorSet where
  (PrimitiveComparatorSet compsl) <> (PrimitiveComparatorSet compsr) =
    PrimitiveComparatorSet $ compsl <> compsr

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

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) =
  any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version comparatorSet =
  doesVersionSatisfyPrimitiveComparatorSet version $
    comparatorSetToPrimitiveComparatorSet comparatorSet

doesVersionSatisfyPrimitiveComparatorSet :: Version -> PrimitiveComparatorSet -> Bool
doesVersionSatisfyPrimitiveComparatorSet version (PrimitiveComparatorSet comps) =
  all (doesVersionSatisfyPrimitiveComparator version) comps

doesVersionSatisfyPrimitiveComparator :: Version -> PrimitiveComparator -> Bool
doesVersionSatisfyPrimitiveComparator version (PrimitiveComparator operator compVersion) =
  case operator of
    Equal -> version == compVersion
    LessThan -> version < compVersion
    LessThanOrEqual -> version <= compVersion
    GreaterThan -> version > compVersion
    GreaterThanOrEqual -> version >= compVersion

comparatorSetToPrimitiveComparatorSet :: ComparatorSet -> PrimitiveComparatorSet
comparatorSetToPrimitiveComparatorSet (ComparatorSet comps) =
  foldl1 (<>) $ comparatorToPrimitiveComparatorSet <$> comps

comparatorToPrimitiveComparatorSet :: Comparator -> PrimitiveComparatorSet
comparatorToPrimitiveComparatorSet (ComparatorP comp) =
  PrimitiveComparatorSet $ fromList [comp]
comparatorToPrimitiveComparatorSet (ComparatorS comp) =
  specialComparatorToPrimitiveComparatorSet comp

specialComparatorToPrimitiveComparatorSet :: SpecialComparator -> PrimitiveComparatorSet
specialComparatorToPrimitiveComparatorSet (Caret version) =
  PrimitiveComparatorSet $
    NE.fromList
      [ PrimitiveComparator GreaterThanOrEqual version,
        PrimitiveComparator LessThan (nextBreakingChangeVersion version)
      ]

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0

-- Helper methods.

(&) :: ComparatorSet -> ComparatorSet -> ComparatorSet
(&) = (<>)

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

caret :: Version -> ComparatorSet
caret = ComparatorSet . pure . ComparatorS . Caret

mkPrimCompSet :: Operator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . ComparatorP . PrimitiveComparator op
