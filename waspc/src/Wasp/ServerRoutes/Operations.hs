module Wasp.ServerRoutes.Operations
  ( operationsRouteInRootRouter,
    operationRouteInOperationsRouter,
    operationRoute,
    getOperationRoutes,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.ServerRoutes.ServerRoute (ServerRoute, ServerRouteOwner (..), makeWaspRouteInNestedRouter)
import Wasp.Util (camelToKebabCase)

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

operationRouteInOperationsRouter :: AS.Operation.Operation -> String
operationRouteInOperationsRouter = camelToKebabCase . AS.Operation.getName

operationRoute :: AS.Operation.Operation -> ServerRoute
operationRoute operation =
  makeWaspRouteInNestedRouter
    (OperationRoute $ AS.Operation.getName operation)
    AS.Api.POST
    [operationsRouteInRootRouter, operationRouteInOperationsRouter operation]

getOperationRoutes :: AppSpec -> [ServerRoute]
getOperationRoutes = map operationRoute . AS.getOperations
