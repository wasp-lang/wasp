module Wasp.Psl.Generator.Schema
  ( generateSchemaBlock,
  )
where

import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Generator.ConfigBlock (generateConfigBlockKeyValues)
import Wasp.Psl.Generator.Enum (generateEnumBody)
import Wasp.Psl.Generator.Model (generateModelBody)

type PslSource = String

generateSchemaBlock :: Psl.Schema.Block -> PslSource
generateSchemaBlock = \case
  Psl.Schema.ModelBlock (Psl.Model.Model name body) -> "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.EnumBlock (Psl.Enum.Enum name values) -> "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
  Psl.Schema.DatasourceBlock (Psl.ConfigBlock.Datasource name content) -> "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
  Psl.Schema.GeneratorBlock (Psl.ConfigBlock.Generator name content) -> "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
