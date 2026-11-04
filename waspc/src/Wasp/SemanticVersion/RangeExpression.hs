{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wasp.SemanticVersion.RangeExpression
  ( PrimitiveOperator (..),
    RangeExpression (..),
    SimpleRangeExpression (..),
    isVersionInRangeExpression,
    rangeExpressionParser,
    simpleRangeExpressionParser,
    hyphenRangeExpressionParser,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), partialVersionParser)
import Wasp.SemanticVersion.Version
  ( Version (..),
    hasSameCoreAs,
    isPreRelease,
    lowestPreReleaseOf,
    lowestVersion,
    releaseVersion,
  )
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    intervalIntersection,
    isVersionInInterval,
    noVersionInterval,
  )

-- | A range expression is either a set of simple range expressions or a hyphen range expression.
data RangeExpression
  = SimpleRangeExpressionSet (NE.NonEmpty SimpleRangeExpression)
  | HyphenRangeExpression PartialVersion PartialVersion
  deriving (Eq, TH.Lift)

-- | A simple range expression is composed of an operator and a partial version.
-- Simple because all operators here require only a single partial version.
-- NOTE: X-Range is already supported on all operators through the 'PartialVersion' implementation.
data SimpleRangeExpression
  = -- | 1.2.3 (=1.2.3), >1.2.3, <1.2.3, >=1.2.3, <=1.2.3
    PrimitiveRangeExpression PrimitiveOperator PartialVersion
  | -- | ~1.2.3
    TildeRangeExpression PartialVersion
  | -- | ^1.2.3
    CaretRangeExpression PartialVersion
  deriving (Eq, TH.Lift)

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce a valid `node-semver` output.
instance Show RangeExpression where
  show (SimpleRangeExpressionSet simpleRangeExpressions) = unwords $ show <$> NE.toList simpleRangeExpressions
  show (HyphenRangeExpression lower upper) = show lower ++ " - " ++ show upper

-- | We rely on this 'show' implementation to produce a valid `node-semver` output.
instance Show SimpleRangeExpression where
  show (PrimitiveRangeExpression primitiveOperator pv) = show primitiveOperator ++ show pv
  show (TildeRangeExpression pv) = "~" ++ show pv
  show (CaretRangeExpression pv) = "^" ++ show pv

-- | We rely on this 'show' implementation to produce a valid `node-semver` output.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

-- | We define concatenation of two range expressions as their intersection.
-- 'HyphenRangeExpression' can't be combined with other range expressions.
instance Semigroup RangeExpression where
  (SimpleRangeExpressionSet left) <> (SimpleRangeExpressionSet right) = SimpleRangeExpressionSet $ NE.nub $ left <> right
  (HyphenRangeExpression _ _) <> _ = error "Cannot intersect HyphenRangeExpression with other range expressions"
  _ <> (HyphenRangeExpression _ _) = error "Cannot intersect HyphenRangeExpression with other range expressions"

instance HasVersionBounds RangeExpression where
  versionBounds (SimpleRangeExpressionSet simpleRangeExpressions) =
    foldr1 intervalIntersection $ versionBounds <$> simpleRangeExpressions
  versionBounds (HyphenRangeExpression lower upper) = (toXRangeLowerBound lower, toXRangeUpperBound upper)

instance HasVersionBounds SimpleRangeExpression where
  versionBounds (PrimitiveRangeExpression primitiveOperator pv) = case primitiveOperator of
    Equal -> (toXRangeLowerBound pv, toXRangeUpperBound pv)
    LessThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive lowestVersion, Exclusive $ lowestPreReleaseOf mjr 0 0)
      (MajorMinor mjr mnr) -> (Inclusive lowestVersion, Exclusive $ lowestPreReleaseOf mjr mnr 0)
      (FullVersion version) -> (Inclusive lowestVersion, Exclusive version)
    LessThanOrEqual -> (Inclusive lowestVersion, toXRangeUpperBound pv)
    GreaterThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive $ releaseVersion (mjr + 1) 0 0, Inf)
      (MajorMinor mjr mnr) -> (Inclusive $ releaseVersion mjr (mnr + 1) 0, Inf)
      (FullVersion version) -> (Exclusive version, Inf)
    GreaterThanOrEqual -> (toXRangeLowerBound pv, Inf)
  versionBounds (TildeRangeExpression pv) = (toXRangeLowerBound pv, toTildeRangeUpperBound pv)
  versionBounds (CaretRangeExpression pv) = (toXRangeLowerBound pv, toCaretRangeUpperBound pv)

-- | Tilde range allows patch-level changes if minor is specified.
toTildeRangeUpperBound :: PartialVersion -> VersionBound
toTildeRangeUpperBound Any = Inf
toTildeRangeUpperBound (Major mjr) = Exclusive $ lowestPreReleaseOf (mjr + 1) 0 0
toTildeRangeUpperBound (MajorMinor mjr mnr) = Exclusive $ lowestPreReleaseOf mjr (mnr + 1) 0
toTildeRangeUpperBound (FullVersion version) = Exclusive $ lowestPreReleaseOf version.major (version.minor + 1) 0

-- | Caret range allows changes that don't modify the leftmost non-zero digit.
toCaretRangeUpperBound :: PartialVersion -> VersionBound
toCaretRangeUpperBound Any = Inf
toCaretRangeUpperBound (Major 0) = Exclusive $ lowestPreReleaseOf 1 0 0
toCaretRangeUpperBound (Major mjr) = Exclusive $ lowestPreReleaseOf (mjr + 1) 0 0
toCaretRangeUpperBound (MajorMinor 0 0) = Exclusive $ lowestPreReleaseOf 0 1 0
toCaretRangeUpperBound (MajorMinor 0 mnr) = Exclusive $ lowestPreReleaseOf 0 (mnr + 1) 0
toCaretRangeUpperBound (MajorMinor mjr _) = Exclusive $ lowestPreReleaseOf (mjr + 1) 0 0
toCaretRangeUpperBound (FullVersion (Version 0 0 ptc _ _)) = Exclusive $ lowestPreReleaseOf 0 0 (ptc + 1)
toCaretRangeUpperBound (FullVersion (Version 0 mnr _ _ _)) = Exclusive $ lowestPreReleaseOf 0 (mnr + 1) 0
toCaretRangeUpperBound (FullVersion version) = Exclusive $ lowestPreReleaseOf (version.major + 1) 0 0

toXRangeUpperBound :: PartialVersion -> VersionBound
toXRangeUpperBound Any = Inf
toXRangeUpperBound (Major mjr) = Exclusive $ lowestPreReleaseOf (mjr + 1) 0 0
toXRangeUpperBound (MajorMinor mjr mnr) = Exclusive $ lowestPreReleaseOf mjr (mnr + 1) 0
toXRangeUpperBound (FullVersion version) = Inclusive version

toXRangeLowerBound :: PartialVersion -> VersionBound
toXRangeLowerBound Any = Inclusive lowestVersion
toXRangeLowerBound (Major mjr) = Inclusive $ releaseVersion mjr 0 0
toXRangeLowerBound (MajorMinor mjr mnr) = Inclusive $ releaseVersion mjr mnr 0
toXRangeLowerBound (FullVersion version) = Inclusive version

-- | Follows `node-semver`'s `satisfies` for a single range expression (comparator set).
-- Besides being in the range expression's bounds, a pre-release version must have
-- the same major, minor and patch as some pre-release version written in the range expression.
-- E.g. >1.2.3-alpha.3 includes 1.2.3-alpha.7, but not 3.4.5-alpha.9.
-- See: https://github.com/npm/node-semver#prerelease-tags
isVersionInRangeExpression :: Version -> RangeExpression -> Bool
isVersionInRangeExpression version rangeExpression =
  isVersionInInterval (versionBounds rangeExpression) version
    && (not (isPreRelease version) || any (hasSameCoreAs version) preReleaseVersions)
  where
    preReleaseVersions :: [Version]
    preReleaseVersions = [fullVersion | FullVersion fullVersion <- partialVersions rangeExpression, isPreRelease fullVersion]

-- | Returns all partial versions written in the range expression.
partialVersions :: RangeExpression -> [PartialVersion]
partialVersions (SimpleRangeExpressionSet simpleRangeExpressions) = partialVersionOf <$> NE.toList simpleRangeExpressions
  where
    partialVersionOf :: SimpleRangeExpression -> PartialVersion
    partialVersionOf (PrimitiveRangeExpression _ pv) = pv
    partialVersionOf (TildeRangeExpression pv) = pv
    partialVersionOf (CaretRangeExpression pv) = pv
partialVersions (HyphenRangeExpression lower upper) = [lower, upper]

-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
rangeExpressionParser :: P.Parsec String () RangeExpression
rangeExpressionParser =
  P.choice
    [ P.try hyphenRangeExpressionParser,
      P.try simpleRangeExpressionSetParser,
      emptyRangeParser
    ]
  where
    simpleRangeExpressionSetParser :: P.Parsec String () RangeExpression
    simpleRangeExpressionSetParser = do
      first <- simpleRangeExpressionParser
      rest <- P.many $ P.try (P.many1 P.space *> simpleRangeExpressionParser)
      pure $ SimpleRangeExpressionSet $ NE.fromList (first : rest)

    -- `node-semver` parses empty input as the equals any range (*).
    emptyRangeParser :: P.Parsec String () RangeExpression
    emptyRangeParser = (SimpleRangeExpressionSet . pure $ PrimitiveRangeExpression Equal Any) <$ P.eof

-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
simpleRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
simpleRangeExpressionParser =
  P.choice
    [ tildeRangeExpressionParser,
      caretRangeExpressionParser,
      primitiveRangeExpressionParser
    ]
  where
    tildeRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
    tildeRangeExpressionParser = TildeRangeExpression <$> (P.char '~' *> P.spaces *> partialVersionParser)

    caretRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
    caretRangeExpressionParser = CaretRangeExpression <$> (P.char '^' *> P.spaces *> partialVersionParser)

    -- See `primitive` definition here: https://github.com/npm/node-semver#range-grammar
    primitiveRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
    primitiveRangeExpressionParser =
      PrimitiveRangeExpression <$> primitiveOperatorParser <* P.spaces <*> partialVersionParser

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

-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeExpressionParser :: P.Parsec String () RangeExpression
hyphenRangeExpressionParser = do
  lower <- partialVersionParser
  _ <- hyphenRangeSeparatorParser
  upper <- partialVersionParser
  pure $ HyphenRangeExpression lower upper
  where
    -- Must have exactly 1 white space character around the hyphen.
    hyphenRangeSeparatorParser :: P.Parsec String () Char
    hyphenRangeSeparatorParser = P.space *> P.char '-' <* P.space
