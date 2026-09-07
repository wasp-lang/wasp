module Wasp.AppSpec.Identifier
  ( isValidWaspIdentifier,
  )
where

import Data.Char (isAlpha, isAlphaNum)

-- | Checks if a string is a valid Wasp identifier.
--
-- A valid identifier starts with a Unicode letter or underscore, continues
-- with Unicode letters, numbers or underscores, may end with any number of apostrophes, and is not one of
-- the reserved keywords.
isValidWaspIdentifier :: String -> Bool
isValidWaspIdentifier str = matchesIdentifierRule str && str `notElem` reservedKeywords
  where
    matchesIdentifierRule [] = False
    matchesIdentifierRule (c : cs) =
      isIdentStart c && all isIdentChar body && all (== '\'') primes
      where
        (body, primes) = span (/= '\'') cs
    isIdentStart ch = isAlpha ch || ch == '_'
    isIdentChar ch = isAlphaNum ch || ch == '_'
    reservedKeywords = ["import", "from", "true", "false"]
