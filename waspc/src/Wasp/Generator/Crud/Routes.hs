module Wasp.Generator.Crud.Routes where

import Data.List (intercalate)
import qualified Wasp.AppSpec.Crud as AS.Crud

getPath :: AS.Crud.CrudOperation -> String
getPath operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "getAll"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

makeRoute :: String -> AS.Crud.CrudOperation -> String
makeRoute name operation = intercalate "/" [getCrudOperationsExpressNamespace, getCrudOperationExpressRoute name, getPath operation]

getCrudOperationsExpressNamespace :: String
getCrudOperationsExpressNamespace = "crud"

getCrudOperationExpressRoute :: String -> String
getCrudOperationExpressRoute = id
