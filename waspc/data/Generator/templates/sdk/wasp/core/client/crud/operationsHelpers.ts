import { useAction, useQuery } from "../operations/hooks.js"
import type { Query, Action } from "../operations/rpc.js"
import type { Tail } from "../../universal/types.js"

// PRIVATE API
export function makeUseQueryFor<Input, Output>(
  query: Query<Input, Output>
) {
  return (
    ...rest: Tail<Parameters<typeof useQuery<Input, Output>>>
  ) => useQuery<Input, Output>(query, ...rest);
}

// PRIVATE API
export function makeUseActionFor<Input = unknown, Output = unknown>(
  action: Action<Input, Output>
) {
  return (
    ...rest: Tail<Parameters<typeof useAction<Input, Output>>>
  ) => useAction<Input, Output>(action, ...rest);
}
