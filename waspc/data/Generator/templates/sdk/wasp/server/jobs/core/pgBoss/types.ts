import { PrismaDelegate } from '../../../_types/index.js'
import type { JSONValue, JSONObject } from '../../../../core/serialization/index.js'

// PRIVATE API
export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
> = (data: Input, context: { entities: Entities }) => Promise<Output>
