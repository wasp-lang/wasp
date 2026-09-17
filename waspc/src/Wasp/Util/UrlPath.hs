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

-- | Whether the first path is the second one or an ancestor of it, going segment by segment.
--
-- >>> "/api" `isPathSegmentPrefixOf` "/api/tasks"
-- True
-- >>> "/api" `isPathSegmentPrefixOf` "/apis"
-- False
-- >>> "/" `isPathSegmentPrefixOf` "/api"
-- True
isPathSegmentPrefixOf :: String -> String -> Bool
isPathSegmentPrefixOf prefixPath path = getPathSegments prefixPath `isPrefixOf` getPathSegments path

-- | The leading segments of an Express route pattern that are plain text, e.g. "/files" for
-- "/files/:id". Everything the pattern can match sits under this prefix.
--
-- >>> getStaticPathPrefix "/files/:id/raw"
-- "/files"
-- >>> getStaticPathPrefix "/:id"
-- "/"
getStaticPathPrefix :: String -> String
getStaticPathPrefix = makePathFromSegments . takeWhile isStaticSegment . getPathSegments
  where
    isStaticSegment = not . any (`elem` expressPatternChars)
    expressPatternChars = ":*{}" :: String

getPathSegments :: String -> [String]
getPathSegments = filter (not . null) . splitOn "/"

makePathFromSegments :: [String] -> String
makePathFromSegments segments = "/" ++ intercalate "/" segments
