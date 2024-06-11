module Wasp.Psl.Generator.ConfigBlock
  ( makeConfigBlockJson,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock

makeConfigBlockJson :: Psl.ConfigBlock.IsConfigBlock a => [(String, String)] -> a -> Value
makeConfigBlockJson overrideValues configBlock =
  object
    [ "name" .= name,
      "keyValues" .= (makeKeyValueJson <$> keyValues)
    ]
  where
    name = Psl.ConfigBlock.getConfigBlockName configBlock
    configBlockValues =
      Psl.ConfigBlock.getConfigBlockKeyValues configBlock
        <&> (\(Psl.ConfigBlock.ConfigBlockKeyValue key value) -> (key, value))
    keyValues = nubBy ((==) `on` fst) (overrideValues ++ configBlockValues)

    makeKeyValueJson :: (String, String) -> Value
    makeKeyValueJson (key, value) = object ["key" .= key, "value" .= value]
