import { callOperation } from '../operations'
import {
  registerActionInProgress,
  registerActionDone,
} from '../operations/resources'

export function createAction(actionRoute, entitiesUsed) {
  async function internalAction(args, optimisticUpdateTuples) {
    registerActionInProgress(optimisticUpdateTuples)
    try {
      // The `return await` is not redundant here. If we removed the await, the
      // `finally` block would execute before the action finishes, prematurely
      // registering the action as done.
      return await callOperation(actionRoute, args)
    } finally {
      await registerActionDone(entitiesUsed, optimisticUpdateTuples)
    }
  }

  const action = (args) => internalAction(args, [])
  action.internal = internalAction

  return action
}
