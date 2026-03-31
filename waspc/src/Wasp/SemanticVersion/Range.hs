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
data Range = Range [RangeExpression]
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

doesVersionSatisfyComparatorSet :: Version -> RangeExpression -> Bool
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
caretRange = Range . pure . Simple . NE.fromList . pure . CaretRange . versionToPartialVersion

backwardsCompatibleWith :: Version -> Range
backwardsCompatibleWith = caretRange

tildeRange :: Version -> Range
tildeRange = Range . pure . Simple . NE.fromList . pure . TildeRange . versionToPartialVersion

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
mkComparatorRange op = Range . pure . Simple . NE.fromList . pure . Primitive op . versionToPartialVersion

r :: TH.QuasiQuoter
r = quasiQuoterFromParser parseRange

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse (rangeParser <* P.eof) ""

-- | Parses a version range.
-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeParser :: P.Parsec String () Range
rangeParser = Range <$> rangeSetParser
  where
    rangeSetParser :: P.Parsec String () [RangeExpression]
    rangeSetParser = do
      first <- rangeSetItemParser
      rest <- P.many $ P.try (logicalOrParser *> rangeSetItemParser)
      pure (first : rest)

    rangeSetItemParser :: P.Parsec String () RangeExpression
    rangeSetItemParser = P.spaces *> rangeExpressionParser <* P.spaces

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = void (P.spaces *> P.string "||" <* P.spaces)

-- | A range expression is either a set of simple range expressions or a hyphen range.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
data RangeExpression
  = Simple (NE.NonEmpty SimpleRangeExpression)
  | HyphenRange PartialVersion PartialVersion
  deriving (Eq, TH.Lift)

-- | A simple range expression is composed of an operator and a partial version.
-- Simple because all operators here require only a single partial version.
--
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
-- NOTE: X-Range is already supported on all operators through the 'PartialVersion' implementation.
data SimpleRangeExpression
  = -- | 1.2.3 (=1.2.3), >1.2.3, <1.2.3, >=1.2.3, <=1.2.3
    Primitive PrimitiveOperator PartialVersion
  | -- | ~1.2.3
    TildeRange PartialVersion
  | -- | ^1.2.3
    CaretRange PartialVersion
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` output.
instance Show RangeExpression where
  show (Simple simpleRangeExpressions) = unwords $ show <$> NE.toList simpleRangeExpressions
  show (HyphenRange lower upper) = show lower ++ " - " ++ show upper

-- | We rely on this 'show' implementation to produce valid `node-semver` output.
instance Show SimpleRangeExpression where
  show (Primitive primOp pv) = show primOp ++ show pv
  show (TildeRange pv) = "~" ++ show pv
  show (CaretRange pv) = "^" ++ show pv

-- | We define concatenation of two comparator sets as a union of their range expressions.
-- Hyphen Ranges can't be combined with other comparator sets.
instance Semigroup RangeExpression where
  (Simple left) <> (Simple right) = Simple $ NE.nub $ left <> right
  (HyphenRange _ _) <> _ = error "Cannot combine Hyphen Range with other comparator sets"
  _ <> (HyphenRange _ _) = error "Cannot combine Hyphen Range with other comparator sets"

instance HasVersionBounds RangeExpression where
  versionBounds (Simple simpleRangeExpressions) =
    foldr1 intervalIntersection $ versionBounds <$> simpleRangeExpressions
  versionBounds (HyphenRange lower upper) = (toXRangeLowerBound lower, toXRangeUpperBound upper)

instance HasVersionBounds SimpleRangeExpression where
  versionBounds (Primitive primOp pv) = case primOp of
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

-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
rangeExpressionParser :: P.Parsec String () RangeExpression
rangeExpressionParser =
  P.choice
    [ P.try hyphenRangeParser,
      P.try simpleSetParser,
      emptyRangeParser
    ]
  where
    simpleSetParser :: P.Parsec String () RangeExpression
    simpleSetParser = do
      first <- simpleRangeExpressionParser
      rest <- P.many $ P.try (P.many1 P.space *> simpleRangeExpressionParser)
      pure $ Simple (NE.fromList (first : rest))

    -- `node-semver` parses empty input as the equals any comparator (*).
    emptyRangeParser :: P.Parsec String () RangeExpression
    emptyRangeParser = (Simple . pure $ Primitive Equal Any) <$ P.eof

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

    -- See `primitive` definition here: https://github.com/npm/node-semver#ran`ge-grammar
    primitiveParser :: P.Parsec String () SimpleRangeExpression
    primitiveParser =
      Primitive <$> primitiveOperatorParser <* P.spaces <*> partialVersionParser

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

-- | Parses a hyphen range.
-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeParser :: P.Parsec String () RangeExpression
hyphenRangeParser = do
  lower <- partialVersionParser
  _ <- hyphenRangeSeparatorParser
  upper <- partialVersionParser
  pure $ HyphenRange lower upper
  where
    -- Must must exactly 1 white space character around the hyphen.
    hyphenRangeSeparatorParser :: P.Parsec String () Char
    hyphenRangeSeparatorParser = P.space *> P.char '-' <* P.space

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

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
