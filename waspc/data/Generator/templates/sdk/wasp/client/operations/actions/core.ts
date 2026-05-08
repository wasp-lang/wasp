import type { _Awaited, _ReturnType } from 'wasp/universal/types'
import type { OperationRpcFor, GenericBackendOperation } from '../rpc.js'
import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  registerActionInProgress,
  registerActionDone,
} from '../internal/resources.js'

// PRIVATE API
export function createAction<BackendAction extends GenericBackendOperation>(
  relativeActionRoute: string,
  entitiesUsed: unknown[]
): ActionFor<BackendAction> {
  const actionRoute = makeOperationRoute(relativeActionRoute)

  async function internalAction(args, specificOptimisticUpdateDefinitions) {
    registerActionInProgress(specificOptimisticUpdateDefinitions)
    try {
      // The `return await` is not redundant here. If we removed the await, the
      // `finally` block would execute before the action finishes, prematurely
      // registering the action as done.
      return await callOperation(actionRoute, args)
    } finally {
      await registerActionDone(entitiesUsed, specificOptimisticUpdateDefinitions)
    }
  }

  // We expose (and document) a restricted version of the API for our users,
  // while also attaching the full "internal" API to the exposed action. By
  // doing this, we can easily use the internal API of an action a users passes
  // into our system (e.g., through the `useAction` hook) without needing a
  // lookup table.
  //
  // While it does technically allow our users to access the interal API, it
  // shouldn't be a problem in practice. Still, if it turns out to be a problem,
  // we can always hide it using a Symbol.
  const action = (args) => internalAction(args, [])
  action.internal = internalAction

  // NOTE: I'm not sure why unknown is necessary here,
  // it might have something to do with functions allowing evolving types,
  // // while objects to not:
  // https://www.typescriptlang.org/play/?#code/MYewdgzgLgBCBGArGBeGBvGBDAXDA5AGYgj4wC+AUAogHTyoHxYBO+llA9JzIQK5hgUAJbhsAG3EgA7hGxgYAUwBuIccuFgA5jCgBPAA6KY8PrBqKhMACYhFcsCCiVQkWPwVoAFAEpUAPgJiUkoPekZ8ZjYgA
  return action as unknown as ActionFor<BackendAction>
}

// PRIVATE API
export type ActionFor<BackendAction extends GenericBackendOperation> =
  OperationRpcFor<BackendAction>
