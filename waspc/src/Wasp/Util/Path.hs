module Wasp.Util.Path
  ( removePathPrefix,
  )
where

import Data.List (isPrefixOf)

removePathPrefix :: FilePath -> FilePath -> Maybe FilePath
removePathPrefix pathPrefix path =
  if isPathPrefixedWithPrefix
    then Just $ drop (length prefixWithTrailingSlash) path
    else Nothing
  where
    isPathPrefixedWithPrefix = prefixWithTrailingSlash `isPrefixOf` path
    prefixWithTrailingSlash = ensureTrailingSlash pathPrefix

ensureTrailingSlash :: String -> String
ensureTrailingSlash path =
  if last path == '/'
    then path
    else path ++ "/"
