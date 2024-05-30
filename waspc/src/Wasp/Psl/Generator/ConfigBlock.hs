module Wasp.Psl.Generator.ConfigBlock
  ( makeConfigBlockJson,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nubBy)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

makeConfigBlockJson :: Psl.Ast.IsConfigBlock a => [(String, String)] -> a -> Value
makeConfigBlockJson defaultValues configBlock =
  object
    [ "name" .= name,
      "keyValues" .= (makeKeyValueJson <$> keyValues)
    ]
  where
    name = Psl.Ast.getConfigBlockName configBlock
    configBlockValues =
      Psl.Ast.getConfigBlockKeyValues configBlock
        <&> (\(Psl.Ast.ConfigBlockKeyValue key value) -> (key, value))
    keyValues = nubBy ((==) `on` fst) (defaultValues ++ configBlockValues)

    makeKeyValueJson :: (String, String) -> Value
    makeKeyValueJson (key, value) = object ["key" .= key, "value" .= value]
