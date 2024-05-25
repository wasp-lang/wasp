module Wasp.Psl.Generator.ConfigBlock
  ( showPrismaDbExtensions,
    showPrismaPreviewFeatures,
  )
where

import Data.List (find)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

showPrismaDbExtensions :: AppSpec -> Maybe String
showPrismaDbExtensions =
  findPrismaConfigBlockKeyValue "extensions"
    . concatMap (\(Psl.Ast.Datasource _ keyValues) -> keyValues)
    . Psl.Ast.getDatasources
    . AS.getPrismaSchema

showPrismaPreviewFeatures :: AppSpec -> Maybe String
showPrismaPreviewFeatures =
  findPrismaConfigBlockKeyValue "previewFeatures"
    . concatMap (\(Psl.Ast.Generator _ keyValues) -> keyValues)
    . Psl.Ast.getGenerators
    . AS.getPrismaSchema

findPrismaConfigBlockKeyValue :: String -> [Psl.Ast.ConfigBlockKeyValue] -> Maybe String
findPrismaConfigBlockKeyValue needle =
  fmap (\(Psl.Ast.ConfigBlockKeyValue _ value) -> value)
    . find (\(Psl.Ast.ConfigBlockKeyValue key _) -> key == needle)
