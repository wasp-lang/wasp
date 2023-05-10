module Wasp.Generator.Crud where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Aeson.Types (Pair)
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

getCrudOperationJson :: String -> AS.Crud.Crud -> Maybe PslModel.Field -> Data.Aeson.Value
getCrudOperationJson name crud primaryField =
  object
    [ "name" .= name,
      -- TODO: Typescript types
      "entityUpper" .= crudEntityName,
      "entityLower" .= Util.toLowerFirst crudEntityName,
      "entitiesArray" .= makeJsArrayFromHaskellList [crudEntityName],
      "primaryFieldName" .= PslModel._name (fromJust primaryField),
      "operationsData" .= object (map getDataForOperation crudOperationsKeys)
    ]
  where
    crudEntityName = AS.refName $ AS.Crud.entity crud

    getDataForOperation :: Crud.AS.CrudOperation -> Pair
    getDataForOperation operation =
      toJsonKey operation
        .= object
          [ "isEnabled" .= (operation `elem` enabledOperations),
            "route" .= Routes.makeRoute name operation,
            "path" .= Routes.getPath operation,
            "isPublic" .= (operation `elem` publicOperations)
          ]

    enabledOperations =
      fromMaybe crudOperationsKeys $
        getEnabledOperationsFromOnly (AS.Crud.only crud) <|> getEnabledOperationsFromExcept (AS.Crud.except crud)
    publicOperations = fromMaybe [] (AS.Crud.public crud)

getCrudEntityPrimaryField :: AS.AppSpec -> AS.Crud.Crud -> Maybe PslModel.Field
getCrudEntityPrimaryField spec crud = Entity.getPrimaryField crudEntity
  where
    crudEntity = snd $ AS.resolveRef spec (AS.Crud.entity crud)

getEnabledOperationsFromOnly :: Maybe [Crud.AS.CrudOperation] -> Maybe [Crud.AS.CrudOperation]
getEnabledOperationsFromOnly = id

getEnabledOperationsFromExcept :: Maybe [Crud.AS.CrudOperation] -> Maybe [Crud.AS.CrudOperation]
getEnabledOperationsFromExcept except = except >>= \except' -> Just $ filter (`notElem` except') crudOperationsKeys

crudOperationsKeys :: [Crud.AS.CrudOperation]
crudOperationsKeys = [AS.Crud.Get, AS.Crud.GetAll, AS.Crud.Create, AS.Crud.Update, AS.Crud.Delete]

toJsonKey :: Crud.AS.CrudOperation -> T.Text
toJsonKey = T.pack . show
