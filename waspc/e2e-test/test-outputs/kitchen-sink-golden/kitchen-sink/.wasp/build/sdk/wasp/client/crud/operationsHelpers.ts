import { useAction, useQuery } from "../operations"
import type { Query, Action } from "../operations/rpc"
import type { Tail } from "../../universal/types"

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
