{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wasp.SemanticVersion.Range where

import Control.Monad (guard, void)
import Data.List (intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), partialVersionParser, versionToPartialVersion)
import Wasp.SemanticVersion.Version (Version (..), nextBreakingChangeVersion)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    allVersionsInterval,
    intervalIntersection,
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    noVersionInterval,
    versionFromBound,
  )
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Comparator sets can be joined by "||" to form a range,
-- which is satisfied by satisfying any of the comparator sets it includes.
data Range = Range [ComparatorSet]
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as a union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ nub $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

instance HasVersionBounds Range where
  versionBounds (Range []) = allVersionsInterval
  versionBounds (Range compSets) = foldr1 intervalUnion $ versionBounds <$> compSets

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version compSet =
  isVersionInInterval (versionBounds compSet) version

doesVersionRangeAllowMajorChanges :: Range -> Bool
doesVersionRangeAllowMajorChanges = not . doesVersionRangeAllowOnlyMinorChanges
  where
    doesVersionRangeAllowOnlyMinorChanges versionRange = isJust $ do
      let versionInterval = versionBounds versionRange
      let lowerBound = fst versionInterval
      lowerBoundVersion <- versionFromBound lowerBound
      let noMajorChangesInterval =
            (lowerBound, Exclusive $ nextBreakingChangeVersion lowerBoundVersion)
      guard $ versionInterval `isSubintervalOf` noMajorChangesInterval

-- Helper methods for constructing a 'Range'.

caretRange :: Version -> Range
caretRange = Range . pure . SimpleComparatorSet . NE.fromList . pure . CaretRange . versionToPartialVersion

backwardsCompatibleWith :: Version -> Range
backwardsCompatibleWith = caretRange

tildeRange :: Version -> Range
tildeRange = Range . pure . SimpleComparatorSet . NE.fromList . pure . TildeRange . versionToPartialVersion

approximatelyEquivalentTo :: Version -> Range
approximatelyEquivalentTo = tildeRange

hyphenRange :: Version -> Version -> Range
hyphenRange v1 v2 = Range [HyphenRange (versionToPartialVersion v1) (versionToPartialVersion v2)]

lt :: Version -> Range
lt = mkComparatorRange LessThan

lte :: Version -> Range
lte = mkComparatorRange LessThanOrEqual

gt :: Version -> Range
gt = mkComparatorRange GreaterThan

gte :: Version -> Range
gte = mkComparatorRange GreaterThanOrEqual

eq :: Version -> Range
eq = mkComparatorRange Equal

mkComparatorRange :: PrimitiveOperator -> Version -> Range
mkComparatorRange op = Range . pure . SimpleComparatorSet . NE.fromList . pure . Primitive . Comparator op . versionToPartialVersion

r :: TH.QuasiQuoter
r = quasiQuoterFromParser parseRange

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse (rangeParser <* P.eof) ""

-- | Parses a version range.
-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeParser :: P.Parsec String () Range
rangeParser = Range <$> rangeSetParser
  where
    rangeSetParser :: P.Parsec String () [ComparatorSet]
    rangeSetParser = do
      first <- rangeExpressionParser
      rest <- P.many $ P.try (logicalOrParser *> rangeExpressionParser)
      pure (first : rest)

    rangeExpressionParser :: P.Parsec String () ComparatorSet
    rangeExpressionParser = P.spaces *> (comparatorSetParser P.<|> emptyRangeParser) <* P.spaces

    -- `node-semver` parses empty input as the equals any comparator (*).
    emptyRangeParser :: P.Parsec String () ComparatorSet
    emptyRangeParser = (SimpleComparatorSet . pure . Primitive $ Comparator Equal Any) <$ P.eof

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = void (P.spaces *> P.string "||" <* P.spaces)

-- | A comparator set is either a sequence of simple range expressions or a hyphen range.
--
-- The name comparator set comes from the fact that all items of comparator set
-- can be desguared into comparators.
data ComparatorSet
  = SimpleComparatorSet (NE.NonEmpty SimpleRangeExpression)
  | HyphenRange PartialVersion PartialVersion
  deriving (Eq, TH.Lift)

-- | Anything that is either a comparator, or can be desugared into comparators.
-- Simple because all operators here require only a single partial version.
--
-- X-Range is already supported on all operators through the 'PartialVersion' implementation.
data SimpleRangeExpression
  = -- | 1.2.3 (=1.2.3), >1.2.3, <1.2.3, >=1.2.3, <=1.2.3
    Primitive Comparator
  | -- | ~1.2.3
    TildeRange PartialVersion
  | -- | ^1.2.3
    CaretRange PartialVersion
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator set.
instance Show ComparatorSet where
  show (SimpleComparatorSet simpleRangeExpressions) = unwords $ show <$> NE.toList simpleRangeExpressions
  show (HyphenRange lower upper) = show lower ++ " - " ++ show upper

-- | We rely on this 'show' implementation to produce valid `node-semver` simple comparator.
instance Show SimpleRangeExpression where
  show (Primitive comparator) = show comparator
  show (TildeRange pv) = "~" ++ show pv
  show (CaretRange pv) = "^" ++ show pv

-- | We define concatenation of two comparator sets as a union of their range expressions.
-- Hyphen Ranges can't be combined with other comparator sets.
instance Semigroup ComparatorSet where
  (SimpleComparatorSet left) <> (SimpleComparatorSet right) = SimpleComparatorSet $ NE.nub $ left <> right
  (HyphenRange _ _) <> _ = error "Cannot combine Hyphen Range with other comparator sets"
  _ <> (HyphenRange _ _) = error "Cannot combine Hyphen Range with other comparator sets"

instance HasVersionBounds ComparatorSet where
  versionBounds (SimpleComparatorSet simpleRangeExpressions) =
    foldr1 intervalIntersection $ versionBounds <$> simpleRangeExpressions
  versionBounds (HyphenRange lower upper) = (toXRangeLowerBound lower, toXRangeUpperBound upper)

instance HasVersionBounds SimpleRangeExpression where
  versionBounds (Primitive comp) = versionBounds comp
  versionBounds (TildeRange pv) = (toXRangeLowerBound pv, toTildeRangeUpperBound pv)
  versionBounds (CaretRange pv) = (toXRangeLowerBound pv, toCareRangetUpperBound pv)

-- | Tilde range allows patch-level changes if minor is specified.
toTildeRangeUpperBound :: PartialVersion -> VersionBound
toTildeRangeUpperBound Any = Inf
toTildeRangeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toTildeRangeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toTildeRangeUpperBound (MajorMinorPatch mjr mnr _) = Exclusive (Version mjr (mnr + 1) 0)

-- | Caret range allows changes that don't modify the leftmost non-zero digit.
toCareRangetUpperBound :: PartialVersion -> VersionBound
toCareRangetUpperBound Any = Inf
toCareRangetUpperBound (Major 0) = Exclusive (Version 1 0 0)
toCareRangetUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toCareRangetUpperBound (MajorMinor 0 0) = Exclusive (Version 0 1 0)
toCareRangetUpperBound (MajorMinor 0 mnr) = Exclusive (Version 0 (mnr + 1) 0)
toCareRangetUpperBound (MajorMinor mjr _) = Exclusive (Version (mjr + 1) 0 0)
toCareRangetUpperBound (MajorMinorPatch 0 0 ptc) = Exclusive (Version 0 0 (ptc + 1))
toCareRangetUpperBound (MajorMinorPatch 0 mnr _) = Exclusive (Version 0 (mnr + 1) 0)
toCareRangetUpperBound (MajorMinorPatch mjr _ _) = Exclusive (Version (mjr + 1) 0 0)

-- | Parses a comparator set.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
comparatorSetParser :: P.Parsec String () ComparatorSet
comparatorSetParser =
  P.choice
    [ P.try hyphenRangeParser,
      P.try simpleComparatorSetParser
    ]
  where
    simpleComparatorSetParser :: P.Parsec String () ComparatorSet
    simpleComparatorSetParser = do
      first <- simpleRangeExpressionParser
      rest <- P.many $ P.try (P.many1 P.space *> simpleRangeExpressionParser)
      pure $ SimpleComparatorSet (NE.fromList (first : rest))

-- | Parses a simple range expression.
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
simpleRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
simpleRangeExpressionParser =
  P.choice
    [ tildeRangeParser,
      caretRangeParser,
      primitiveParser
    ]
  where
    tildeRangeParser :: P.Parsec String () SimpleRangeExpression
    tildeRangeParser = TildeRange <$> (P.char '~' *> P.spaces *> partialVersionParser)

    caretRangeParser :: P.Parsec String () SimpleRangeExpression
    caretRangeParser = CaretRange <$> (P.char '^' *> P.spaces *> partialVersionParser)

    primitiveParser :: P.Parsec String () SimpleRangeExpression
    primitiveParser = Primitive <$> comparatorParser

-- | Parses a hyphen range.
-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeParser :: P.Parsec String () ComparatorSet
hyphenRangeParser = do
  lower <- partialVersionParser
  _ <- hyphenRangeSeparatorParser
  upper <- partialVersionParser
  pure $ HyphenRange lower upper
  where
    -- Must must exactly 1 white space character around the hyphen.
    hyphenRangeSeparatorParser :: P.Parsec String () Char
    hyphenRangeSeparatorParser = P.space *> P.char '-' <* P.space

-- | A comparator composed of an operator and a partial version.
-- It represents a single version constraint in a range.
data Comparator
  = Comparator PrimitiveOperator PartialVersion
  deriving (Eq, TH.Lift)

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show Comparator where
  show (Comparator op pv) = show op ++ show pv

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

instance HasVersionBounds Comparator where
  versionBounds (Comparator primOp pv) = case primOp of
    Equal -> (toXRangeLowerBound pv, toXRangeUpperBound pv)
    LessThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr 0 0)
      (MajorMinor mjr mnr) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr mnr 0)
      (MajorMinorPatch mjr mnr ptc) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr mnr ptc)
    LessThanOrEqual -> (Inclusive $ Version 0 0 0, toXRangeUpperBound pv)
    GreaterThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive $ Version (mjr + 1) 0 0, Inf)
      (MajorMinor mjr mnr) -> (Inclusive $ Version mjr (mnr + 1) 0, Inf)
      (MajorMinorPatch mjr mnr ptc) -> (Exclusive $ Version mjr mnr ptc, Inf)
    GreaterThanOrEqual -> (toXRangeLowerBound pv, Inf)

toXRangeUpperBound :: PartialVersion -> VersionBound
toXRangeUpperBound Any = Inf
toXRangeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toXRangeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toXRangeUpperBound (MajorMinorPatch mjr mnr ptc) = Inclusive (Version mjr mnr ptc)

toXRangeLowerBound :: PartialVersion -> VersionBound
toXRangeLowerBound Any = Inclusive $ Version 0 0 0
toXRangeLowerBound (Major mjr) = Inclusive $ Version mjr 0 0
toXRangeLowerBound (MajorMinor mjr mnr) = Inclusive $ Version mjr mnr 0
toXRangeLowerBound (MajorMinorPatch mjr mnr ptc) = Inclusive $ Version mjr mnr ptc

-- | Parses a single comparator.
-- See `primitive` definition here: https://github.com/npm/node-semver#ran`ge-grammar
comparatorParser :: P.Parsec String () Comparator
comparatorParser = primitiveComparatorParser
  where
    primitiveComparatorParser :: P.Parsec String () Comparator
    primitiveComparatorParser =
      Comparator <$> primitiveOperatorParser <* P.spaces <*> partialVersionParser

    primitiveOperatorParser :: P.Parsec String () PrimitiveOperator
    primitiveOperatorParser =
      P.choice
        [ LessThanOrEqual <$ P.try (P.string "<="),
          GreaterThanOrEqual <$ P.try (P.string ">="),
          LessThan <$ P.char '<',
          GreaterThan <$ P.char '>',
          Equal <$ P.char '=',
          Equal <$ P.string ""
        ]
