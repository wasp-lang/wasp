module Wasp.SemanticVersion.Parsers
  ( naturalNumberParser,
  )
where

import Control.Applicative ((<|>))
import Numeric.Natural (Natural)
import qualified Text.Parsec as P

-- | Parses natural numbers without leading zeroes.
-- See `<numeric identifier>` here: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
naturalNumberParser :: P.Parsec String () Natural
naturalNumberParser =
  read <$> (zeroParser <|> noLeadingZeroNaturalNumberParser)
  where
    zeroParser :: P.Parsec String () String
    zeroParser = P.try (P.string "0" <* P.notFollowedBy P.digit)

    noLeadingZeroNaturalNumberParser :: P.Parsec String () String
    noLeadingZeroNaturalNumberParser = (:) <$> P.oneOf ['1' .. '9'] <*> P.many P.digit
