module Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
    simpleComparatorParser,
    hyphenComparatorParser,
  )
where

import Text.Parsec (Parsec, (<?>))
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    partialVersionParser,
    toCaretUpperBound,
    toLowerBound,
    toTildeUpperBound,
    toUpperBound,
  )
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (Exclusive, Inclusive, Inf),
  )

-- | A 'Comparator' is composed of an operator and a partial version.
-- It represents a single version constraint in a 'Range'.
data Comparator
  = PrimitiveComparator PrimitiveOperator PartialVersion
  | -- | Caret range (^) allows changes that don't modify leftmost non-zero digit.
    BackwardsCompatibleWith PartialVersion
  | -- | Tilde range (~) allows patch-level changes if minor is specified.
    ApproximatelyEquvivalentTo PartialVersion
  | -- | X-Range specifies a "stand in" for numeric values.
    -- Any of "X", "x" or "*" are valid.
    -- A partial version range is treated as an X-Range.
    XRange PartialVersion
  | -- | Hyphen range specifies an inclusive set.
    HyphenRange PartialVersion PartialVersion
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid node-semver comparator.
instance Show Comparator where
  show (PrimitiveComparator op pv) = show op ++ show pv
  show (BackwardsCompatibleWith pv) = "^" ++ show pv
  show (ApproximatelyEquvivalentTo pv) = "~" ++ show pv
  show (XRange pv) = show pv
  show (HyphenRange pv1 pv2) = show pv1 ++ " - " ++ show pv2

instance HasVersionBounds Comparator where
  versionBounds (PrimitiveComparator operator pv) =
    case operator of
      Equal ->
        -- =1.2.3 means exactly 1.2.3
        -- =1.2 means >=1.2.0 <1.3.0
        -- =1 means >=1.0.0 <2.0.0
        (Inclusive (toLowerBound pv), toUpperBound pv)
      LessThan ->
        -- <1.2.3 means <1.2.3
        -- <1.2 means <1.2.0
        -- <1 means <1.0.0
        (Inf, Exclusive (toLowerBound pv))
      LessThanOrEqual ->
        -- <=1.2.3 means <=1.2.3
        -- <=1.2 means <1.3.0
        -- <=1 means <2.0.0
        case pv of
          Full m n p -> (Inf, Inclusive (Version m n p))
          _ -> (Inf, toUpperBound pv)
      GreaterThan ->
        -- >1.2.3 means >1.2.3
        -- >1.2 means >=1.3.0
        -- >1 means >=2.0.0
        case pv of
          Full m n p -> (Exclusive (Version m n p), Inf)
          _ -> case toUpperBound pv of
            Exclusive v -> (Inclusive v, Inf)
            Inclusive v -> (Exclusive v, Inf)
            Inf -> (Inf, Inf)
      GreaterThanOrEqual ->
        -- >=1.2.3 means >=1.2.3
        -- >=1.2 means >=1.2.0
        -- >=1 means >=1.0.0
        (Inclusive (toLowerBound pv), Inf)
  versionBounds (BackwardsCompatibleWith rv) =
    (Inclusive (toLowerBound rv), toCaretUpperBound rv)
  versionBounds (ApproximatelyEquvivalentTo rv) =
    (Inclusive (toLowerBound rv), toTildeUpperBound rv)
  versionBounds (XRange rv) =
    (Inclusive (toLowerBound rv), toUpperBound rv)
  versionBounds (HyphenRange lower upper) =
    let lowerBound = Inclusive (toLowerBound lower)
        upperBound = case upper of
          Full m n p -> Inclusive (Version m n p)
          _ -> toUpperBound upper
     in (lowerBound, upperBound)

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid node-semver comparator.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

-- | Parses a hyphen range (e.g. "1.2.3 - 2.3.4").
-- Separated from 'simpleComparatorParser' because hyphen ranges cannot be
-- combined with other comparators in a comparator set.
hyphenComparatorParser :: Parsec String () Comparator
hyphenComparatorParser = HyphenRange <$> partialVersionParser <*> (P.space *> P.char '-' *> P.space *> partialVersionParser)

-- | Parses a single non-hyphen comparator (primitive, tilde, caret, or x-range).
simpleComparatorParser :: Parsec String () Comparator
simpleComparatorParser =
  P.choice
    [ emptyInputIsAnyVersionParser,
      P.try tildeParser,
      P.try caretParser,
      P.try primitiveParser,
      xRangeParser
    ]
    <?> "comparator"
  where
    emptyInputIsAnyVersionParser :: Parsec String () Comparator
    emptyInputIsAnyVersionParser = XRange Any <$ P.lookAhead P.eof

    tildeParser :: Parsec String () Comparator
    tildeParser = ApproximatelyEquvivalentTo <$> (P.char '~' *> partialVersionParser)

    caretParser :: Parsec String () Comparator
    caretParser = BackwardsCompatibleWith <$> (P.char '^' *> partialVersionParser)

    xRangeParser :: Parsec String () Comparator
    xRangeParser = XRange <$> partialVersionParser

    primitiveParser :: Parsec String () Comparator
    primitiveParser = PrimitiveComparator <$> primitiveOperatorParser <*> partialVersionParser

    primitiveOperatorParser :: Parsec String () PrimitiveOperator
    primitiveOperatorParser =
      P.choice
        [ LessThanOrEqual <$ P.try (P.string "<="),
          GreaterThanOrEqual <$ P.try (P.string ">="),
          LessThan <$ P.char '<',
          GreaterThan <$ P.char '>',
          Equal <$ P.char '='
        ]
        <?> "operator"
