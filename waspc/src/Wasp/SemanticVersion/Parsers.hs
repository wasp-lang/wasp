module Wasp.SemanticVersion.Parsers
  ( noLeadingZeroNaturalP,
  )
where

import Control.Applicative ((<|>))
import Numeric.Natural (Natural)
import Text.Parsec (Parsec, digit, many, notFollowedBy, oneOf, string)

noLeadingZeroNaturalP :: Parsec String () Natural
noLeadingZeroNaturalP = do
  digitString <- zeroP <|> noLeadingZeroNumberP
  -- Ensure we consumed all digits. E.g., fail on "01".
  notFollowedBy digit
  pure (read digitString)
  where
    zeroP = string "0"
    noLeadingZeroNumberP = (:) <$> oneOf ['1' .. '9'] <*> many digit
