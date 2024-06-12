module Wasp.Psl.Ast.Schema
  ( Schema (..),
    Block (..),
    getModels,
    getEnums,
    getDatasources,
    getGenerators,
  )
where

import Wasp.Psl.Ast.ConfigBlock (Datasource, Generator)
import Wasp.Psl.Ast.Enum (Enum)
import Wasp.Psl.Ast.Model (Model)
import Prelude hiding (Enum)

data Schema = Schema [Block]
  deriving (Show, Eq)

data Block
  = ModelBlock Model
  | EnumBlock Enum
  | DatasourceBlock Datasource
  | GeneratorBlock Generator
  deriving (Show, Eq)

getModels :: Schema -> [Model]
getModels (Schema elements) = [model | ModelBlock model <- elements]

getEnums :: Schema -> [Enum]
getEnums (Schema elements) = [enum | EnumBlock enum <- elements]

getDatasources :: Schema -> [Datasource]
getDatasources (Schema elements) = [datasource | DatasourceBlock datasource <- elements]

getGenerators :: Schema -> [Generator]
getGenerators (Schema elements) = [generator | GeneratorBlock generator <- elements]
