module Wasp.Util.UrlPath
  ( stripTrailingSlashes,
  )
where

import Data.List (dropWhileEnd)

-- | Removes trailing slashes, but keeps the root path as "/".
--
-- >>> stripTrailingSlashes "/api/"
-- "/api"
-- >>> stripTrailingSlashes "/"
-- "/"
stripTrailingSlashes :: String -> String
stripTrailingSlashes path = case dropWhileEnd (== '/') path of
  "" -> "/"
  stripped -> stripped
