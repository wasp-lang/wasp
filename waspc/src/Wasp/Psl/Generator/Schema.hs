module Wasp.Psl.Generator.Schema
  ( generateSchema,
    generateSchemaBlock,
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
import Wasp.Psl.Generator.WithCtx (generateWithCtx)
import Wasp.Util (indent, trim)

generateSchema :: Psl.Schema.Schema -> PslSource
generateSchema (Psl.Schema.Schema blocks) = unlines $ generateWithCtx generateSchemaBlock <$> blocks

generateSchemaBlock :: Psl.Schema.Block -> PslSource
generateSchemaBlock = \case
  Psl.Schema.ModelBlock modelBlock -> generateModel modelBlock
  Psl.Schema.ViewBlock viewBlock -> generateView viewBlock
  Psl.Schema.TypeBlock typeBlock -> generateType typeBlock
  Psl.Schema.EnumBlock enumBlock -> generateEnum enumBlock
  Psl.Schema.ConfigBlock configBlock -> generateConfigBlock configBlock

generateModel :: Psl.Model.Model -> PslSource
generateModel (Psl.Model.Model name body) = generateBlock "model" name $ generateModelBody body

generateView :: Psl.View.View -> PslSource
generateView (Psl.View.View name body) = generateBlock "view" name $ generateModelBody body

generateType :: Psl.Type.Type -> PslSource
generateType (Psl.Type.Type name body) = generateBlock "type" name $ generateModelBody body

generateEnum :: Psl.Enum.Enum -> PslSource
generateEnum (Psl.Enum.Enum name values) = generateBlock "enum" name $ generateEnumBody values

generateConfigBlock :: Psl.ConfigBlock.ConfigBlock -> PslSource
generateConfigBlock (Psl.ConfigBlock.ConfigBlock configType name content) =
  generateBlock blockType name $ generateConfigBlockKeyValuePairs content
  where
    blockType = case configType of
      Psl.ConfigBlock.Datasource -> "datasource"
      Psl.ConfigBlock.Generator -> "generator"

-- | Common structure for all top-level blocks in the schema.
generateBlock :: String -> String -> PslSource -> PslSource
generateBlock blockType name body =
  unlines $
    [blockType ++ " " ++ name ++ " {"]
      ++ [indent 2 $ trim body]
      ++ ["}"]
