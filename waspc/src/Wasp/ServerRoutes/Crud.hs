module Wasp.ServerRoutes.Crud
  ( crudRouteInRootRouter,
    getCrudOperationRouterRoute,
    crudOperationRouteInCrudRouter,
    crudOperationRoute,
    getCrudRoutes,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.ServerRoutes.ServerRoute (ServerRoute, ServerRouteOwner (..), makeWaspRoute)

crudRouteInRootRouter :: String
crudRouteInRootRouter = "crud"

getCrudOperationRouterRoute :: String -> String
getCrudOperationRouterRoute crudName = crudName

crudOperationRouteInCrudRouter :: AS.Crud.CrudOperation -> String
crudOperationRouteInCrudRouter operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "get-all"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

crudOperationRoute :: String -> AS.Crud.CrudOperation -> ServerRoute
crudOperationRoute crudName crudOperation =
  makeWaspRoute
    (CrudRoute crudName)
    AS.Api.POST
    [crudRouteInRootRouter, getCrudOperationRouterRoute crudName, crudOperationRouteInCrudRouter crudOperation]

getCrudRoutes :: AppSpec -> [ServerRoute]
getCrudRoutes spec =
  [ crudOperationRoute crudName crudOperation
  | (crudName, crud) <- AS.getCruds spec,
    (crudOperation, _) <- AS.Crud.toOperationList (AS.Crud.operations crud)
  ]
