import { useAction, useQuery } from "../operations"
import type { Tail } from "../../universal/types"

// PRIVATE API
export function makeUseQueryFor<Input, Output>(
  query: Parameters<typeof useQuery<Input, Output>>[0]
) {
  return (
    ...rest: Tail<Parameters<typeof useQuery<Input, Output>>>
  ) => useQuery<Input, Output>(query, ...rest);
}

// PRIVATE API
export function makeUseActionFor<Input = unknown, Output = unknown>(
  action: Parameters<typeof useAction<Input, Output>>[0]
) {
  return (
    ...rest: Tail<Parameters<typeof useAction<Input, Output>>>
  ) => useAction<Input, Output>(action, ...rest);
}
