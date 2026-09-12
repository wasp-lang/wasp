-- | The routes the Wasp server registers itself, ahead of the user's apis.
-- Both the generators and the validators build paths from these, so they live in one place.
module Wasp.ServerRoutes
  ( authRouteInRootRouter,
    operationsRouteInRootRouter,
    crudRouteInRootRouter,
    healthRouteInRootRouter,
    webSocketRouteInRootRouter,
    operationRouteInOperationsRouter,
    crudOperationRouteInCrudRouter,
    getCrudOperationRouterRoute,
    makeCrudOperationFullPath,
  )
where

import Data.List (intercalate)
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Util (camelToKebabCase)

authRouteInRootRouter :: String
authRouteInRootRouter = "auth"

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

crudRouteInRootRouter :: String
crudRouteInRootRouter = "crud"

healthRouteInRootRouter :: String
healthRouteInRootRouter = "health"

-- | socket.io's default path, which both the server and the client use.
webSocketRouteInRootRouter :: String
webSocketRouteInRootRouter = "socket.io"

operationRouteInOperationsRouter :: AS.Operation.Operation -> String
operationRouteInOperationsRouter = camelToKebabCase . AS.Operation.getName

crudOperationRouteInCrudRouter :: AS.Crud.CrudOperation -> String
crudOperationRouteInCrudRouter operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "get-all"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

getCrudOperationRouterRoute :: String -> String
getCrudOperationRouterRoute crudOperationName = crudOperationName

makeCrudOperationFullPath :: String -> AS.Crud.CrudOperation -> String
makeCrudOperationFullPath crudOperationName crudOperation =
  intercalate "/" [crudRouteInRootRouter, getCrudOperationRouterRoute crudOperationName, crudOperationRouteInCrudRouter crudOperation]
