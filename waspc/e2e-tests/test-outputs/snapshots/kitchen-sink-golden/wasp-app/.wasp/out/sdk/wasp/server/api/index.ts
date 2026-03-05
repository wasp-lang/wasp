
import { type ParamsDictionary as ExpressParams, type Query as ExpressQuery } from 'express-serve-static-core'

import {
  type _Task,
  type Api,
  type AuthenticatedApi,
} from '../_types'


// PUBLIC API
export type FooBar<
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> =
  AuthenticatedApi<
    [
      _Task,
    ],
    P,
    ResBody,
    ReqBody,
    ReqQuery,
    Locals
  >
export type BarBaz<
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> =
  Api<
    [
      _Task,
    ],
    P,
    ResBody,
    ReqBody,
    ReqQuery,
    Locals
  >
export type WebhookCallback<
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> =
  Api<
    [
    ],
    P,
    ResBody,
    ReqBody,
    ReqQuery,
    Locals
  >
export type StreamingText<
  P extends ExpressParams = ExpressParams,
  ResBody = any,
  ReqBody = any,
  ReqQuery extends ExpressQuery = ExpressQuery,
  Locals extends Record<string, any> = Record<string, any>
> =
  AuthenticatedApi<
    [
    ],
    P,
    ResBody,
    ReqBody,
    ReqQuery,
    Locals
  >
