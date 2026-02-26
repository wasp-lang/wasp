module Wasp.SemanticVersion.Parsers
  ( noLeadingZeroNaturalP,
  )
where

import Control.Applicative ((<|>))
import Numeric.Natural (Natural)
import Text.Parsec (Parsec, digit, many, notFollowedBy, oneOf, string, try)

noLeadingZeroNaturalP :: Parsec String () Natural
noLeadingZeroNaturalP = do
  read <$> (zeroP <|> noLeadingZeroNumberP)
  where
    zeroP = try (string "0" <* notFollowedBy digit)
    noLeadingZeroNumberP = (:) <$> oneOf ['1' .. '9'] <*> many digit
