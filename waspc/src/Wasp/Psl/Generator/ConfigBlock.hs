module Wasp.Psl.Generator.ConfigBlock
  ( generateConfigBlockKeyValues,
    overrideConfigBlockValues,
  )
where

import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock

generateConfigBlockKeyValues :: [Psl.ConfigBlock.ConfigBlockKeyValue] -> String
generateConfigBlockKeyValues keyValues = unlines . map ("  " ++) $ generateConfigBlockKeyValue <$> keyValues
  where
    generateConfigBlockKeyValue (Psl.ConfigBlock.ConfigBlockKeyValue key value) = key ++ " = " ++ value

overrideConfigBlockValues :: [(String, String)] -> Psl.ConfigBlock.ConfigBlock -> Psl.ConfigBlock.ConfigBlock
overrideConfigBlockValues
  overridePairs
  (Psl.ConfigBlock.ConfigBlock configBlockType name originalKeyValues) =
    Psl.ConfigBlock.ConfigBlock configBlockType name overridenKeyValues
    where
      configBlockPairs =
        originalKeyValues
          <&> (\(Psl.ConfigBlock.ConfigBlockKeyValue key value) -> (key, value))
      overridenKeyValues =
        nubBy ((==) `on` fst) (overridePairs ++ configBlockPairs)
          <&> uncurry Psl.ConfigBlock.ConfigBlockKeyValue
