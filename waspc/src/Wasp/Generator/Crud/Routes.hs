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
makeFullPath name operation = intercalate "/" [getCrudOperationsExpressNamespace, getCrudOperationExpressRoute name, getRoute operation]

getCrudOperationsExpressNamespace :: String
getCrudOperationsExpressNamespace = "crud"

getCrudOperationExpressRoute :: String -> String
getCrudOperationExpressRoute = id
