module Wasp.Util.UrlPath
  ( stripTrailingSlashes,
    isPathSegmentPrefixOf,
    getStaticPathPrefix,
  )
where

import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.List.Split (splitOn)

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

-- | The static beginning of an Express path pattern: all segments before the first segment
-- containing an Express pattern character (@:*(){}?+@).
--
-- >>> getStaticPathPrefix "/foo/bar"
-- "/foo/bar"
-- >>> getStaticPathPrefix "/foo/:id/edit"
-- "/foo"
-- >>> getStaticPathPrefix "/:id"
-- "/"
getStaticPathPrefix :: String -> String
getStaticPathPrefix path = "/" ++ intercalate "/" staticSegments
  where
    staticSegments = takeWhile isStaticSegment $ filter (not . null) $ splitOn "/" path
    isStaticSegment = not . any (`elem` expressPatternChars)
    expressPatternChars = ":*(){}?+" :: String
