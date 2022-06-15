{{={= =}=}}
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

async function {= actionFnName =}(args) {
  resources.registerActionInProgress(entitiesUsed)
  const actionResult = await callOperation('{= actionRoute =}', args)
  await resources.registerActionDone(entitiesUsed)
  return actionResult
}

export const entitiesUsed = {=& entitiesArray =}

export default {= actionFnName =}


