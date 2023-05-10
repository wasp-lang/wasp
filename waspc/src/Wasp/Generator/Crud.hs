module Wasp.Generator.Crud where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Aeson.Types (Pair)
import Data.List ((\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Crud as Crud.AS
import qualified Wasp.AppSpec.Entity as Entity
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.Crud.Routes as Routes
import qualified Wasp.Psl.Ast.Model as PslModel
import qualified Wasp.Util as Util

availableCrudOperations :: [Crud.AS.CrudOperation]
availableCrudOperations = [AS.Crud.Get, AS.Crud.GetAll, AS.Crud.Create, AS.Crud.Update, AS.Crud.Delete]

getCrudOperationJson :: String -> AS.Crud.Crud -> Maybe PslModel.Field -> Data.Aeson.Value
getCrudOperationJson name crud primaryField =
  object
    [ "name" .= name,
      -- TODO: Typescript types
      "entityUpper" .= crudEntityName,
      "entityLower" .= Util.toLowerFirst crudEntityName,
      "entitiesArray" .= makeJsArrayFromHaskellList [crudEntityName],
      -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
      "primaryFieldName" .= PslModel._name (fromJust primaryField),
      "operations" .= object (map getDataForOperation availableCrudOperations)
    ]
  where
    crudEntityName = AS.refName $ AS.Crud.entity crud

    getDataForOperation :: Crud.AS.CrudOperation -> Pair
    getDataForOperation operation =
      toJsonKey operation
        .= object
          [ "isEnabled" .= (operation `elem` enabledOperations),
            "route" .= Routes.getRoute operation,
            "fullPath" .= Routes.makeFullPath name operation,
            "isPublic" .= (operation `elem` publicOperations)
          ]
    publicOperations = fromMaybe [] (AS.Crud.public crud)
    enabledOperations = getEnabledOperations crud

getEnabledOperations :: AS.Crud.Crud -> [Crud.AS.CrudOperation]
getEnabledOperations crud =
  fromMaybe availableCrudOperations $
    getEnabledOperationsFromOnly (AS.Crud.only crud) <|> getEnabledOperationsFromExcept (AS.Crud.except crud)
  where
    getEnabledOperationsFromOnly = id
    -- \\ operator calculates the difference between two lists
    getEnabledOperationsFromExcept except = (availableCrudOperations \\) <$> except

getCrudEntityPrimaryField :: AS.AppSpec -> AS.Crud.Crud -> Maybe PslModel.Field
getCrudEntityPrimaryField spec crud = Entity.getPrimaryField crudEntity
  where
    crudEntity = snd $ AS.resolveRef spec (AS.Crud.entity crud)

toJsonKey :: Crud.AS.CrudOperation -> T.Text
toJsonKey = T.pack . show
