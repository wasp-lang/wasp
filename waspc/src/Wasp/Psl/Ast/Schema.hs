module Wasp.Psl.Ast.Schema
  ( Schema (..),
    SchemaElement (..),
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

data Schema = Schema [SchemaElement]
  deriving (Show, Eq)

data SchemaElement
  = SchemaModel Model
  | SchemaEnum Enum
  | SchemaDatasource Datasource
  | SchemaGenerator Generator
  deriving (Show, Eq)

getModels :: Schema -> [Model]
getModels (Schema elements) = [model | SchemaModel model <- elements]

getEnums :: Schema -> [Enum]
getEnums (Schema elements) = [enum | SchemaEnum enum <- elements]

getDatasources :: Schema -> [Datasource]
getDatasources (Schema elements) = [datasource | SchemaDatasource datasource <- elements]

getGenerators :: Schema -> [Generator]
getGenerators (Schema elements) = [generator | SchemaGenerator generator <- elements]
