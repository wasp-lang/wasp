module Util
    ( camelToKebabCase
    ) where

import Data.Char (isUpper, toLower)


camelToKebabCase :: String -> String
camelToKebabCase "" = ""
camelToKebabCase camel@(camelHead:camelTail) = kebabHead:kebabTail
  where
    kebabHead = toLower camelHead
    kebabTail = concat $ map
      (\(a, b) -> (if (isCamelHump (a, b)) then ['-'] else []) ++ [toLower b])
      (zip camel camelTail)
    isCamelHump (a, b) = (not . isUpper) a && isUpper b
