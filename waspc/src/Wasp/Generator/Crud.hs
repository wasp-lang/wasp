module Wasp.Generator.Crud where

import Control.Applicative ((<|>))
import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Crud as Crud.AS
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.Crud.Routes as Routes
import qualified Wasp.Util as Util

getCrudOperationJson :: String -> AS.Crud.Crud -> Data.Aeson.Value
getCrudOperationJson name crud =
  object
    [ "name" .= name,
      "enabledOperations" .= object enabledOperationsJson,
      -- TODO: find the id field
      -- TODO: auth
      -- TODO: Typescript types
      "entityUpper" .= entityName,
      "entityLower" .= Util.toLowerFirst entityName,
      "operationsData"
        .= object
          [ getOperationDataPair AS.Crud.Get,
            getOperationDataPair AS.Crud.GetAll,
            getOperationDataPair AS.Crud.Create,
            getOperationDataPair AS.Crud.Update,
            getOperationDataPair AS.Crud.Delete
          ],
      "entitiesArray" .= entitiesArray
    ]
  where
    enabledOperationsJson = map (\key -> toJsonKey key .= (key `elem` enabledOperations)) crudOperationsKeys
    enabledOperations = fromMaybe crudOperationsKeys $ getEnabledOperationsFromOnly only <|> getEnabledOperationsFromExcept except
    only = AS.Crud.only crud
    except = AS.Crud.except crud
    entityName = AS.refName $ AS.Crud.entity crud
    entitiesArray = makeJsArrayFromHaskellList [entityName]

    getOperationDataPair :: Crud.AS.CrudOperation -> Pair
    getOperationDataPair operation = case operation of
      AS.Crud.Get -> toJsonKey AS.Crud.Get .= object ["route" .= Routes.makeGetRoute name, "path" .= Routes.getPath]
      AS.Crud.GetAll -> toJsonKey AS.Crud.GetAll .= object ["route" .= Routes.makeGetAllRoute name, "path" .= Routes.getAllPath]
      AS.Crud.Create -> toJsonKey AS.Crud.Create .= object ["route" .= Routes.makeCreateRoute name, "path" .= Routes.createPath]
      AS.Crud.Update -> toJsonKey AS.Crud.Update .= object ["route" .= Routes.makeUpdateRoute name, "path" .= Routes.updatePath]
      AS.Crud.Delete -> toJsonKey AS.Crud.Delete .= object ["route" .= Routes.makeDeleteRoute name, "path" .= Routes.deletePath]

getEnabledOperationsFromOnly :: Maybe [Crud.AS.CrudOperation] -> Maybe [Crud.AS.CrudOperation]
getEnabledOperationsFromOnly = id

getEnabledOperationsFromExcept :: Maybe [Crud.AS.CrudOperation] -> Maybe [Crud.AS.CrudOperation]
getEnabledOperationsFromExcept except = except >>= \except' -> Just $ filter (`notElem` except') crudOperationsKeys

crudOperationsKeys :: [Crud.AS.CrudOperation]
crudOperationsKeys = [AS.Crud.Get, AS.Crud.GetAll, AS.Crud.Create, AS.Crud.Update, AS.Crud.Delete]

toJsonKey :: Crud.AS.CrudOperation -> T.Text
toJsonKey = T.pack . show
