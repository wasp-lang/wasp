module Wasp.Cli.PnpmWorkspaceFile
  ( removeTopLevelKey,
    appendOverrides,
  )
where

import Data.List (isPrefixOf)

-- This module edits the text of a `pnpm-workspace.yaml`. It handles the shape
-- those files have, top-level keys with indented blocks, not all of YAML.

type YamlText = String

-- | Drops the top-level key and its block.
removeTopLevelKey :: String -> YamlText -> YamlText
removeTopLevelKey topLevelKey = unlines . removeKey . lines
  where
    removeKey [] = []
    removeKey (line : restOfLines)
      | (topLevelKey ++ ":") `isPrefixOf` line = removeKey $ dropWhile isInBlock restOfLines
      | otherwise = line : removeKey restOfLines

    isInBlock line = null line || " " `isPrefixOf` line || "#" `isPrefixOf` line

-- | Adds an `overrides` block of package names and their specifiers. The file
-- must not have one already.
appendOverrides :: [(String, String)] -> YamlText -> YamlText
appendOverrides overrides yamlText =
  yamlText ++ unlines ("overrides:" : map makeOverrideLine overrides)
  where
    makeOverrideLine (packageName, specifier) = "  " ++ show packageName ++ ": " ++ show specifier
