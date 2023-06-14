module Wasp.Util.FilePath
  ( removePathPrefix,
  )
where

import Data.List (isPrefixOf)
import qualified System.FilePath as FP

removePathPrefix :: FilePath -> FilePath -> Maybe FilePath
removePathPrefix pathPrefix path =
  if isPathPrefixedWithPrefix
    then Just $ drop (length prefixWithTrailingSlash) path
    else Nothing
  where
    isPathPrefixedWithPrefix = prefixWithTrailingSlash `isPrefixOf` path
    prefixWithTrailingSlash = ensureTrailingSlash pathPrefix

ensureTrailingSlash :: String -> String
ensureTrailingSlash path | FP.hasTrailingPathSeparator path = path
ensureTrailingSlash path = path ++ [FP.pathSeparator]
