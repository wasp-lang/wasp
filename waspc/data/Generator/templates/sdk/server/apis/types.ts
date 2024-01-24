{{={= =}=}}

import { type ParamsDictionary as ExpressParams, type Query as ExpressQuery } from 'express-serve-static-core'

export type { ParamsDictionary as ExpressParams, Query as ExpressQuery } from 'express-serve-static-core'

import {
  {=# allEntities =}
  type {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedApi =}
  type Api,
  {=/ shouldImportNonAuthenticatedApi =}
  {=# shouldImportAuthenticatedApi =}
  type AuthenticatedApi,
  {=/ shouldImportAuthenticatedApi =}
} from '../_types'

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
