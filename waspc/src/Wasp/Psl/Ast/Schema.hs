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

import Data.Maybe (mapMaybe)
import Wasp.Psl.Ast.ConfigBlock (ConfigBlock)
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Ast.Enum (Enum)
import Wasp.Psl.Ast.Model (Model)
import Wasp.Psl.Ast.Type (Type)
import Wasp.Psl.Ast.View (View)
import Wasp.Psl.Ast.WithCtx (WithCtx (WithCtx))
import Prelude hiding (Enum)

data Schema = Schema [WithCtx Block]
  deriving (Show, Eq)

data Block
  = ModelBlock Model
  | ViewBlock View
  | TypeBlock Type
  | EnumBlock Enum
  | ConfigBlock ConfigBlock
  deriving (Show, Eq)

getModels :: Schema -> [WithCtx Model]
getModels = selectBlocks $ \case
  ModelBlock model -> Just model
  _ -> Nothing

getViews :: Schema -> [WithCtx View]
getViews = selectBlocks $ \case
  ViewBlock view -> Just view
  _ -> Nothing

getTypes :: Schema -> [WithCtx Type]
getTypes = selectBlocks $ \case
  TypeBlock typeName -> Just typeName
  _ -> Nothing

getEnums :: Schema -> [WithCtx Enum]
getEnums = selectBlocks $ \case
  EnumBlock enum -> Just enum
  _ -> Nothing

getDatasources :: Schema -> [WithCtx ConfigBlock]
getDatasources = selectBlocks $ \case
  ConfigBlock configBlock@(Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Datasource _ _) -> Just configBlock
  _ -> Nothing

getGenerators :: Schema -> [WithCtx ConfigBlock]
getGenerators = selectBlocks $ \case
  ConfigBlock configBlock@(Psl.ConfigBlock.ConfigBlock Psl.ConfigBlock.Generator _ _) -> Just configBlock
  _ -> Nothing

selectBlocks :: (Block -> Maybe a) -> Schema -> [WithCtx a]
selectBlocks filterFn (Schema blocks) = mapMaybe (liftMaybe . fmap filterFn) blocks
  where
    liftMaybe :: WithCtx (Maybe a) -> Maybe (WithCtx a)
    liftMaybe (WithCtx (Just a) context) = Just $ WithCtx a context
    liftMaybe (WithCtx Nothing _) = Nothing
