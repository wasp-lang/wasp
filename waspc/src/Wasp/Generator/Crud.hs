module Wasp.Generator.Crud
  ( getCrudOperationJson,
    getCrudEntityPrimaryField,
    getCrudFilePath,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.List ((\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import StrongPath (File', Path', Rel)
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Crud as Crud.AS
import qualified Wasp.AppSpec.Entity as Entity
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.Crud.Routes as Routes
import qualified Wasp.Psl.Ast.Model as PslModel
import qualified Wasp.Util as Util

crudOperations :: [Crud.AS.CrudOperation]
crudOperations = [AS.Crud.Get, AS.Crud.GetAll, AS.Crud.Create, AS.Crud.Update, AS.Crud.Delete]

getCrudOperationJson :: String -> AS.Crud.Crud -> PslModel.Field -> Aeson.Value
getCrudOperationJson crudOperationName crud primaryField =
  object
    [ "name" .= crudOperationName,
      "operations" .= object (map getDataForOperation crudOperations),
      "entityUpper" .= crudEntityName,
      "entityLower" .= Util.toLowerFirst crudEntityName,
      "entitiesArray" .= makeJsArrayFromHaskellList [crudEntityName],
      "primaryFieldName" .= PslModel._name primaryField
    ]
  where
    crudEntityName = AS.refName $ AS.Crud.entity crud

    getDataForOperation :: Crud.AS.CrudOperation -> Aeson.Types.Pair
    getDataForOperation operation =
      key
        .= object
          [ "isEnabled" .= (operation `elem` enabledOperations),
            "route" .= Routes.getRoute operation,
            "fullPath" .= Routes.makeFullPath crudOperationName operation,
            "isPublic" .= (operation `elem` publicOperations)
          ]
      where
        key = T.pack . show $ operation

    publicOperations = fromMaybe [] (AS.Crud.public crud)
    enabledOperations = fromMaybe crudOperations (maybeOperationsBasedOnOnly <|> maybeOperationsBasedOnExcept)

    maybeOperationsBasedOnOnly = AS.Crud.only crud
    -- Diff between all and operations defined in except
    maybeOperationsBasedOnExcept = (crudOperations \\) <$> AS.Crud.except crud

getCrudEntityPrimaryField :: AS.AppSpec -> AS.Crud.Crud -> Maybe PslModel.Field
getCrudEntityPrimaryField spec crud = Entity.getPrimaryField crudEntity
  where
    crudEntity = snd $ AS.resolveRef spec (AS.Crud.entity crud)

getCrudFilePath :: String -> String -> Path' (Rel r) File'
getCrudFilePath crudName ext = fromJust (SP.parseRelFile (crudName ++ "." ++ ext))
