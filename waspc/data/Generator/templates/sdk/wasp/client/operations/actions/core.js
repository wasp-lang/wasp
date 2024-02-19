import { callOperation, makeOperationRoute } from '../internal/index.js'
import {
  registerActionInProgress,
  registerActionDone,
} from '../internal/resources.js'

// PRIVATE API
export function createAction(relativeActionRoute, entitiesUsed) {
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

  return action
}
