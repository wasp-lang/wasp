module Wasp.Psl.Generator.Schema
  ( generateSchemaBlock,
  )
where

import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.Type as Psl.Type
import qualified Wasp.Psl.Ast.View as Psl.View
import Wasp.Psl.Generator.Common (PslSource)
import Wasp.Psl.Generator.ConfigBlock (generateConfigBlockKeyValuePairs)
import Wasp.Psl.Generator.Enum (generateEnumBody)
import Wasp.Psl.Generator.Model (generateModelBody)

generateSchemaBlock :: Psl.Schema.Block -> PslSource
generateSchemaBlock = \case
  Psl.Schema.ModelBlock (Psl.Model.Model name body) -> "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.ViewBlock (Psl.View.View name body) -> "view " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.TypeBlock (Psl.Type.Type name body) -> "type " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.EnumBlock (Psl.Enum.Enum name values) -> "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
  Psl.Schema.ConfigBlock (Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource name content) -> "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValuePairs content ++ "}"
  Psl.Schema.ConfigBlock (Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator name content) -> "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValuePairs content ++ "}"
