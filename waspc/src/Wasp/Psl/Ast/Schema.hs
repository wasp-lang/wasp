module Wasp.Psl.Ast.Schema
  ( Schema (..),
    Block (..),
    getModels,
    getEnums,
    getDatasources,
    getGenerators,
  )
where

import Wasp.Psl.Ast.ConfigBlock (ConfigBlock)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Ast.Enum (Enum)
import Wasp.Psl.Ast.Model (Model)
import Prelude hiding (Enum)

data Schema = Schema [Block]
  deriving (Show, Eq)

data Block
  = ModelBlock Model
  | EnumBlock Enum
  | ConfigBlock ConfigBlock
  deriving (Show, Eq)

getModels :: Schema -> [Model]
getModels (Schema blocks) = [model | ModelBlock model <- blocks]

getEnums :: Schema -> [Enum]
getEnums (Schema blocks) = [enum | EnumBlock enum <- blocks]

getDatasources :: Schema -> [ConfigBlock]
getDatasources schema = [datasource | datasource@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource _ _)) <- getConfigBlocks schema]

getGenerators :: Schema -> [ConfigBlock]
getGenerators schema = [generator | generator@((Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator _ _)) <- getConfigBlocks schema]

getConfigBlocks :: Schema -> [ConfigBlock]
getConfigBlocks (Schema blocks) = [configBlock | ConfigBlock configBlock <- blocks]
