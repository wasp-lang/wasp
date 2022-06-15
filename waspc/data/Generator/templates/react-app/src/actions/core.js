import { callOperation } from '../operations'
import {
  registerActionInProgress,
  registerActionDone,
} from '../operations/resources'

export function createAction(actionRoute, entitiesUsed) {
  async function internalAction(args, optimisticallyUpdatedCacheKeys = []) {
    registerActionInProgress(optimisticallyUpdatedCacheKeys)
    const actionResult = await callOperation(actionRoute, args)
    await registerActionDone(entitiesUsed, optimisticallyUpdatedCacheKeys)
    return actionResult
  }

  const action = (args) => internalAction(args)
  action.internal = internalAction

  return action
}
