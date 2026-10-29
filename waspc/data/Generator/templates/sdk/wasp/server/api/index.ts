{{={= =}=}}

import type { ParamsDictionary as ExpressParams, Query as ExpressQuery } from 'express-serve-static-core'

import type {
  {=# allEntities =}
  {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedApi =}
  Api,
  {=/ shouldImportNonAuthenticatedApi =}
  {=# shouldImportAuthenticatedApi =}
  AuthenticatedApi,
  {=/ shouldImportAuthenticatedApi =}
} from '../_types'


// PUBLIC API
{=# apiRoutes =}
export type {= typeName =}<
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> =
  {=# usesAuth =}
  AuthenticatedApi<
  {=/ usesAuth =}
  {=^ usesAuth =}
  Api<
  {=/ usesAuth =}
    [
    {=# entities =}
      {= internalTypeName =},
    {=/ entities =}
    ],
    P,
    ResBody,
    ReqBody,
    ReqQuery,
    Locals
  >
{=/ apiRoutes =}
