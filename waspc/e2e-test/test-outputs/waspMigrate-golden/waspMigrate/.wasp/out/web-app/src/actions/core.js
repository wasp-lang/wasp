import { callOperation } from '../operations'
import { invalidateQueriesUsing } from '../operations/resources'

export function createAction(actionRoute, entitiesUsed) {
  return async function action(args) {
    const actionResult = await callOperation(actionRoute, args)
    await invalidateQueriesUsing(entitiesUsed)
    return actionResult
  }
}
