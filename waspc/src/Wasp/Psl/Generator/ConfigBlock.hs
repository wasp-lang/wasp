module Wasp.Psl.Generator.ConfigBlock
  ( generateConfigBlockKeyValuePairs,
  )
where

import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Generator.Common (PslSource)

generateConfigBlockKeyValuePairs :: [Psl.ConfigBlock.ConfigBlockKeyValuePair] -> PslSource
generateConfigBlockKeyValuePairs keyValues = unlines . map ("  " ++) $ generateConfigBlockKeyValuePair <$> keyValues
  where
    generateConfigBlockKeyValuePair (Psl.ConfigBlock.ConfigBlockKeyValuePair key value) = key ++ " = " ++ value
