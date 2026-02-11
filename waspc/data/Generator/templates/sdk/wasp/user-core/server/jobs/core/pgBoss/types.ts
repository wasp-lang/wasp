import { PrismaDelegate } from 'wasp/server/_types'
import type { JSONValue, JSONObject } from '../../../../../core/core/serialization/index.js'

// PRIVATE API
export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
> = (data: Input, context: { entities: Entities }) => Promise<Output>
