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

generateSchemaBlock :: Psl.Schema.Block -> String
generateSchemaBlock (Psl.Schema.ModelBlock (Psl.Model.Model name body)) = "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
generateSchemaBlock (Psl.Schema.EnumBlock (Psl.Enum.Enum name values)) = "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
generateSchemaBlock (Psl.Schema.DatasourceBlock (Psl.ConfigBlock.Datasource name content)) = "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
generateSchemaBlock (Psl.Schema.GeneratorBlock (Psl.ConfigBlock.Generator name content)) = "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValues content ++ "}"
