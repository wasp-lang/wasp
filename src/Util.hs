module Util
    ( camelToKebabCase
    , onFirst
    , toLowerFirst
    , toUpperFirst
    , headSafe
    ) where

import Data.Char (isUpper, toLower, toUpper)


camelToKebabCase :: String -> String
camelToKebabCase "" = ""
camelToKebabCase camel@(camelHead:camelTail) = kebabHead:kebabTail
  where
    kebabHead = toLower camelHead
    kebabTail = concat $ map
      (\(a, b) -> (if (isCamelHump (a, b)) then ['-'] else []) ++ [toLower b])
      (zip camel camelTail)
    isCamelHump (a, b) = (not . isUpper) a && isUpper b

-- | Applies given function to the first element of the list.
--   If list is empty, returns empty list.
onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (x:xs) = (f x):xs

toLowerFirst :: String -> String
toLowerFirst = onFirst toLower

toUpperFirst :: String -> String
toUpperFirst = onFirst toUpper

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe xs = Just (head xs)
