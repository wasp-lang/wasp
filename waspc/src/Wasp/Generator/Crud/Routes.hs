module Wasp.Generator.Crud.Routes where

import Data.List (intercalate)
import qualified Wasp.AppSpec.Crud as AS.Crud

getRoute :: AS.Crud.CrudOperation -> String
getRoute operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "getAll"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

makeFullPath :: String -> AS.Crud.CrudOperation -> String
makeFullPath crudOperationName crudOperation = intercalate "/" [crudOperationsRouterNamespace, getCrudOperationRouterRoute crudOperationName, getRoute crudOperation]

crudOperationsRouterNamespace :: String
crudOperationsRouterNamespace = "crud"

getCrudOperationRouterRoute :: String -> String
getCrudOperationRouterRoute crudOperationName = crudOperationName
