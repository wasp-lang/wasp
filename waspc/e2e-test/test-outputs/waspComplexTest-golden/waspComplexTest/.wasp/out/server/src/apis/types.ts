
import { ParamsDictionary as ExpressParams, Query as ExpressQuery } from 'express-serve-static-core'

export { ParamsDictionary as ExpressParams, Query as ExpressQuery } from 'express-serve-static-core'

import {
  type AuthenticatedApi,
} from '../_types'

export type FooBar<
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

