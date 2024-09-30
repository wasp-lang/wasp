module Wasp.Psl.Ast.Schema
  ( Schema (..),
    Block (..),
    getModels,
    getViews,
    getTypes,
    getEnums,
    getDatasources,
    getGenerators,
    getModelNames,
  )
where

import Wasp.Psl.Ast.ConfigBlock (ConfigBlock)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Ast.Enum (Enum)
import Wasp.Psl.Ast.Model (Model)
import qualified Wasp.Psl.Ast.Model as Model
import Wasp.Psl.Ast.Type (Type)
import Wasp.Psl.Ast.View (View)
import Prelude hiding (Enum)

data Schema = Schema [Block]
  deriving (Show, Eq)

data Block
  = ModelBlock Model
  | ViewBlock View
  | TypeBlock Type
  | EnumBlock Enum
  | ConfigBlock ConfigBlock
  deriving (Show, Eq)

getModels :: Schema -> [Model]
getModels (Schema blocks) = [model | ModelBlock model <- blocks]

getViews :: Schema -> [View]
getViews (Schema blocks) = [view | ViewBlock view <- blocks]

getTypes :: Schema -> [Type]
getTypes (Schema blocks) = [typeBlock | TypeBlock typeBlock <- blocks]

getEnums :: Schema -> [Enum]
getEnums (Schema blocks) = [enum | EnumBlock enum <- blocks]

getDatasources :: Schema -> [ConfigBlock]
getDatasources schema = [datasource | datasource@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource _ _)) <- getConfigBlocks schema]

getGenerators :: Schema -> [ConfigBlock]
getGenerators schema = [generator | generator@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator _ _)) <- getConfigBlocks schema]

getConfigBlocks :: Schema -> [ConfigBlock]
getConfigBlocks (Schema blocks) = [configBlock | ConfigBlock configBlock <- blocks]

getModelNames :: Schema -> [String]
getModelNames schema = map Model.getName $ getModels schema
