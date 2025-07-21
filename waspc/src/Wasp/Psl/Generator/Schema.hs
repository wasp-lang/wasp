module Wasp.Psl.Generator.Schema
  ( generateSchemaBlock,
  )
where

import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Ast.OutputNode (OutputNode (OutputNode))
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.Type as Psl.Type
import qualified Wasp.Psl.Ast.View as Psl.View
import Wasp.Psl.Generator.Common (PslSource)
import Wasp.Psl.Generator.ConfigBlock (generateConfigBlockKeyValuePairs)
import Wasp.Psl.Generator.Enum (generateEnumBody)
import Wasp.Psl.Generator.Model (generateModelBody)
import Wasp.Psl.Generator.OutputNode (generateNodeContext)

generateSchemaBlock :: Psl.Schema.Block -> PslSource
generateSchemaBlock = \case
  Psl.Schema.ModelBlock (OutputNode (Psl.Model.Model name body) context) ->
    generateNodeContext context $
      "model " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.ViewBlock (OutputNode (Psl.View.View name body) context) ->
    generateNodeContext context $
      "view " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.TypeBlock (OutputNode (Psl.Type.Type name body) context) ->
    generateNodeContext context $
      "type " ++ name ++ " {\n" ++ generateModelBody body ++ "}"
  Psl.Schema.EnumBlock (OutputNode (Psl.Enum.Enum name values) context) ->
    generateNodeContext context $
      "enum " ++ name ++ " {\n" ++ generateEnumBody values ++ "}"
  Psl.Schema.ConfigBlock (Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource name content) ->
    "datasource " ++ name ++ " {\n" ++ generateConfigBlockKeyValuePairs content ++ "}"
  Psl.Schema.ConfigBlock (Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator name content) ->
    "generator " ++ name ++ " {\n" ++ generateConfigBlockKeyValuePairs content ++ "}"
