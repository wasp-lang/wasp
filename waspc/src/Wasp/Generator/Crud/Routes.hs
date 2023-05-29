module Wasp.Generator.Crud.Routes where

import Data.List (intercalate)
import qualified Wasp.AppSpec.Crud as AS.Crud

getRoute :: AS.Crud.CrudOperation -> String
getRoute operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "get-all"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

makeFullPath :: String -> AS.Crud.CrudOperation -> String
makeFullPath crudOperationName crudOperation = intercalate "/" [getCrudOperationRouterRoute crudOperationName, getRoute crudOperation]

getCrudOperationRouterRoute :: String -> String
getCrudOperationRouterRoute crudOperationName = crudOperationName
