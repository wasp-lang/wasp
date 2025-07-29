module Wasp.Psl.Ast.Schema
  ( Schema (..),
    Block (..),
    getModels,
    getViews,
    getTypes,
    getEnums,
    getDatasources,
    getGenerators,
  )
where

import Wasp.Psl.Ast.ConfigBlock (ConfigBlock)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Ast.Enum (Enum)
import Wasp.Psl.Ast.Model (Model)
import Wasp.Psl.Ast.Type (Type)
import Wasp.Psl.Ast.View (View)
import Wasp.Psl.Ast.WithCtx (WithCtx)
import Prelude hiding (Enum)

data Schema = Schema [Block]
  deriving (Show, Eq)

data Block
  = ModelBlock (WithCtx Model)
  | ViewBlock (WithCtx View)
  | TypeBlock (WithCtx Type)
  | EnumBlock (WithCtx Enum)
  | ConfigBlock ConfigBlock
  deriving (Show, Eq)

getModels :: Schema -> [WithCtx Model]
getModels (Schema blocks) = [model | ModelBlock model <- blocks]

getViews :: Schema -> [WithCtx View]
getViews (Schema blocks) = [view | ViewBlock view <- blocks]

getTypes :: Schema -> [WithCtx Type]
getTypes (Schema blocks) = [typeBlock | TypeBlock typeBlock <- blocks]

getEnums :: Schema -> [WithCtx Enum]
getEnums (Schema blocks) = [enum | EnumBlock enum <- blocks]

getDatasources :: Schema -> [ConfigBlock]
getDatasources schema = [datasource | datasource@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource _ _)) <- getConfigBlocks schema]

getGenerators :: Schema -> [ConfigBlock]
getGenerators schema = [generator | generator@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator _ _)) <- getConfigBlocks schema]

getConfigBlocks :: Schema -> [ConfigBlock]
getConfigBlocks (Schema blocks) = [configBlock | ConfigBlock configBlock <- blocks]
