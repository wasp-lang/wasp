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
    range,
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

data Comparator = ComparatorP PrimitiveComparator | ComparatorS SpecialComparator
  deriving (Eq)

data PrimitiveComparator = PrimitiveComparator Operator Version
  deriving (Eq)

data SpecialComparator = Caret Version
  deriving (Eq)

data ComparatorSet = ComparatorSet (NonEmpty Comparator)
  deriving (Eq)

data PrimitiveComparatorSet = PrimitiveComparatorSet (NonEmpty PrimitiveComparator)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show Comparator where
  show (ComparatorP c) = show c
  show (ComparatorS c) = show c

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show SpecialComparator where
  show (Caret v) = "^" ++ show v

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show PrimitiveComparator where
  show (PrimitiveComparator op v) = show op ++ show v

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

instance Semigroup ComparatorSet where
  (ComparatorSet compsl) <> (ComparatorSet compsr) = ComparatorSet $ compsl <> compsr

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show PrimitiveComparatorSet where
  show (PrimitiveComparatorSet comps) = unwords $ show <$> NE.toList comps

instance Semigroup PrimitiveComparatorSet where
  (PrimitiveComparatorSet compsl) <> (PrimitiveComparatorSet compsr) = PrimitiveComparatorSet $ compsl <> compsr

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

doesVersionSatisfyPrimitiveComparator :: Version -> PrimitiveComparator -> Bool
doesVersionSatisfyPrimitiveComparator version (PrimitiveComparator operator compVersion) = case operator of
  Equal -> version == compVersion
  LessThan -> version < compVersion
  LessThanOrEqual -> version <= compVersion
  GreaterThan -> version > compVersion
  GreaterThanOrEqual -> version >= compVersion

doesVersionSatisfyPrimitiveComparatorSet :: Version -> PrimitiveComparatorSet -> Bool
doesVersionSatisfyPrimitiveComparatorSet version (PrimitiveComparatorSet comps) = all (doesVersionSatisfyPrimitiveComparator version) comps

comparatorSetToPrimitiveComparatorSet :: ComparatorSet -> PrimitiveComparatorSet
comparatorSetToPrimitiveComparatorSet (ComparatorSet comps) = foldl1 (<>) $ comparatorToPrimitiveComparatorSet <$> comps

comparatorToPrimitiveComparatorSet :: Comparator -> PrimitiveComparatorSet
comparatorToPrimitiveComparatorSet (ComparatorP comp) = PrimitiveComparatorSet $ fromList [comp]
comparatorToPrimitiveComparatorSet (ComparatorS comp) = specialComparatorToPrimitiveComparatorSet comp

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version comparatorSet = doesVersionSatisfyPrimitiveComparatorSet version $ comparatorSetToPrimitiveComparatorSet comparatorSet

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

-- Helper methods.

-- The idea is to use them like:
--   Range [lt (Version 1 2 3) <> gt (Version 2 3 0), eq (Version 0 0 1)]
--   or
--   range [lt (Version 1 2 3), gt (Version 2 3 0)] <> range [eq (Version 0 0 1)]
--   which both translate to: <1.2.3 >=2.3.0 || =0.0.1

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

range :: [ComparatorSet] -> Range
range [] = Range []
range compSets = Range [foldl1 (<>) compSets]

mkPrimCompSet :: Operator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . ComparatorP . PrimitiveComparator op
