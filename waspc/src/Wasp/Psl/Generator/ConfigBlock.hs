module Wasp.Psl.Generator.ConfigBlock
  ( generateConfigBlockKeyValuePairs,
  )
where

import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Generator.Common (PslSource)

generateConfigBlockKeyValuePairs :: [Psl.ConfigBlock.KeyValuePair] -> PslSource
generateConfigBlockKeyValuePairs keyValues = unlines . map ("  " ++) $ generateConfigBlockKeyValuePair <$> keyValues
  where
    generateConfigBlockKeyValuePair (Psl.ConfigBlock.KeyValuePair key value) = key ++ " = " ++ value
