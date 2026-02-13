module Wasp.SemanticVersion
  ( -- * Types
    Version (..),
    PartialVersion (..),
    fromVersion,
    Range (..),
    ComparatorSet (..),
    Comparator (..),
    PrimitiveOperator (..),

    -- * Range matching
    isVersionInRange,
    doesVersionRangeAllowMajorChanges,

    -- * ComparatorSet constructors
    lt,
    lte,
    gt,
    gte,
    eq,
    backwardsCompatibleWith,
    approximatelyEquvivalentTo,
    xRange,
    hyphenRange,

    -- * Parsing
    parseRange,
    rangeParser,
  )
where

import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
  )
import Wasp.SemanticVersion.ComparatorSet
  ( ComparatorSet (..),
    approximatelyEquvivalentTo,
    backwardsCompatibleWith,
    eq,
    gt,
    gte,
    hyphenRange,
    lt,
    lte,
    xRange,
  )
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), fromVersion)
import Wasp.SemanticVersion.Range
  ( Range (..),
    doesVersionRangeAllowMajorChanges,
    isVersionInRange,
  )
import Wasp.SemanticVersion.Range.Parser (parseRange, rangeParser)
import Wasp.SemanticVersion.Version (Version (..))
