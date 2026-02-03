module Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
  )
where

import Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
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

-- | A 'Comparator' is composed of an operator and a version.
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

-- | We rely on this `show` implementation to produce valid semver representation of comparator.
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

-- | We rely on this `show` implementation to produce valid semver representation of version.
-- Note: Equal shows as "" because both "=1.2.3" and "1.2.3" are valid semver,
-- and the canonical form omits the "=".
instance Show PrimitiveOperator where
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="
