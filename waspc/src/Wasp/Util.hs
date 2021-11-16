module Wasp.Util
  ( camelToKebabCase,
    onFirst,
    toLowerFirst,
    toUpperFirst,
    headSafe,
    jsonSet,
  )
where

import qualified Data.Aeson as Aeson
import Data.Char (isUpper, toLower, toUpper)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as Text

camelToKebabCase :: String -> String
camelToKebabCase "" = ""
camelToKebabCase camel@(camelHead : camelTail) = kebabHead : kebabTail
  where
    kebabHead = toLower camelHead
    kebabTail =
      concat $
        map
          (\(a, b) -> (if isCamelHump (a, b) then ['-'] else []) ++ [toLower b])
          (zip camel camelTail)
    isCamelHump (a, b) = (not . isUpper) a && isUpper b

-- | Applies given function to the first element of the list.
--   If list is empty, returns empty list.
onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (x : xs) = f x : xs

toLowerFirst :: String -> String
toLowerFirst = onFirst toLower

toUpperFirst :: String -> String
toUpperFirst = onFirst toUpper

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe xs = Just (head xs)

jsonSet :: Text.Text -> Aeson.Value -> Aeson.Value -> Aeson.Value
jsonSet key value (Aeson.Object o) = Aeson.Object $ M.insert key value o
jsonSet _ _ _ = error "Input JSON must be an object"
