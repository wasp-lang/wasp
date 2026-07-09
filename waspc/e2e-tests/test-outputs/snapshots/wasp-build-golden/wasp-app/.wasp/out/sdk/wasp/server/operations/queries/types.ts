
import {
  type Payload,
} from '../../_types/index.js'

// PUBLIC API
// Generic query type for code that can't reference generated per-operation
// types, e.g. Wasp modules. Less precise than the generated types: it knows
// nothing about entities or auth, so the implementor describes the context.
export type Query<Args = unknown, Result = unknown, Context = unknown> = (
  args: Args,
  context: Context,
) => Result | Promise<Result>

