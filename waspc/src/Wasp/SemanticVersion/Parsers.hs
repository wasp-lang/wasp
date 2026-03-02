module Wasp.SemanticVersion.Parsers
  ( naturalNumberParser,
  )
where

import Control.Applicative ((<|>))
import Numeric.Natural (Natural)
import Text.Parsec (Parsec, digit, many, notFollowedBy, oneOf, string, try)

naturalNumberParser :: Parsec String () Natural
naturalNumberParser = do
  read <$> (zeroParser <|> noLeadingZeroNaturalNumberParser)
  where
    zeroParser :: Parsec String () String
    zeroParser = try (string "0" <* notFollowedBy digit)

    noLeadingZeroNaturalNumberParser :: Parsec String () String
    noLeadingZeroNaturalNumberParser = (:) <$> oneOf ['1' .. '9'] <*> many digit
