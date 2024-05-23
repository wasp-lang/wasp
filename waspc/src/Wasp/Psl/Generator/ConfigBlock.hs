module Wasp.Psl.Generator.ConfigBlock
  ( showPrismaDbExtensions,
    showPrismaPreviewFeatures,
  )
where

import Data.List (find)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.Psl.Ast.Model as Psl.Ast

showPrismaDbExtensions :: AppSpec -> Maybe String
showPrismaDbExtensions spec = findPrismaConfigBlockKeyValue "extensions" keyValues
  where
    (Psl.Ast.Schema prismaSchemaElements) = AS.getPrismaSchema spec
    datasources = [datasource | Psl.Ast.SchemaDatasource datasource <- prismaSchemaElements]
    -- We are looking through all datasources
    keyValues = concatMap (\(Psl.Ast.Datasource _ kv) -> kv) datasources

showPrismaPreviewFeatures :: AppSpec -> Maybe String
showPrismaPreviewFeatures spec = findPrismaConfigBlockKeyValue "previewFeatures" keyValues
  where
    (Psl.Ast.Schema prismaSchemaElements) = AS.getPrismaSchema spec
    generators = [generator | Psl.Ast.SchemaGenerator generator <- prismaSchemaElements]
    -- We are looking through all generators
    keyValues = concatMap (\(Psl.Ast.Generator _ kv) -> kv) generators

findPrismaConfigBlockKeyValue :: String -> [Psl.Ast.ConfigBlockKeyValue] -> Maybe String
findPrismaConfigBlockKeyValue key keyValues = do
  keyValue <- find (\(Psl.Ast.ConfigBlockKeyValue key' _) -> key' == key) keyValues
  case keyValue of
    Psl.Ast.ConfigBlockKeyValue _ value -> Just value
