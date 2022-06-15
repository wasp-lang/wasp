{ {={= = }= } }
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

export async function internalAction(args, { optimisticallyUpdatedCacheKeys = [] }) {
  resources.registerActionInProgress(optimisticallyUpdatedCacheKeys)
  const actionResult = await callOperation('{= actionRoute =}', args)
  await resources.registerActionDone(entitiesUsed, optimisticallyUpdatedCacheKeys)
  return actionResult
}

export const entitiesUsed = {=& entitiesArray =}

const action = (args) => internalAction(args, {})
action.internal = internalAction;

export default action
