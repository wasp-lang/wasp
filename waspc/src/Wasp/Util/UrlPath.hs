module Wasp.Util.UrlPath
  ( stripTrailingSlashes,
    toUrlPathPrefix,
    isPathSegmentPrefixOf,
  )
where

import Data.List (dropWhileEnd, isPrefixOf)

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

-- | Turns an absolute URL path into the form used when building paths by concatenation:
-- the root becomes "" and any other path loses its trailing slashes, so that @prefix ++ "/foo"@
-- is always a valid path.
--
-- >>> toUrlPathPrefix "/"
-- ""
-- >>> toUrlPathPrefix "/api/"
-- "/api"
toUrlPathPrefix :: String -> String
toUrlPathPrefix path = case stripTrailingSlashes path of
  "/" -> ""
  stripped -> stripped

-- | Whether the first path is the second path or one of its ancestors, by whole segments.
-- Both paths are expected to be absolute.
--
-- >>> "/api" `isPathSegmentPrefixOf` "/api/auth"
-- True
-- >>> "/api" `isPathSegmentPrefixOf` "/apis"
-- False
-- >>> "/" `isPathSegmentPrefixOf` "/anything"
-- True
isPathSegmentPrefixOf :: String -> String -> Bool
isPathSegmentPrefixOf prefix path =
  normalizedPrefix == normalizedPath
    || normalizedPrefix == "/"
    || (normalizedPrefix ++ "/") `isPrefixOf` normalizedPath
  where
    normalizedPrefix = stripTrailingSlashes prefix
    normalizedPath = stripTrailingSlashes path
