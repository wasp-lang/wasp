import { useQuery } from '../operations/queries/core.js'
import { useAction, type ActionFor } from '../operations/actions/core.js'
import type { Query } from '../operations/queries/core.js'

type Tail<T extends any[]> = T extends [any, ...infer Rest] ? Rest : never

export function makeUseQueryFor<Input, Output>(query: Query<Input, Output>) {
  return (...rest: Tail<Parameters<typeof useQuery<Input, Output>>>) =>
    useQuery<Input, Output>(query, ...rest)
}

export function makeUseActionFor(
  action: ActionFor<(...args: any[]) => any>,
) {
  return (...rest: any[]) => useAction(action, ...rest)
}
