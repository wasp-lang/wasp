{{={= =}=}}
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

async function {= actionFnName =}(args) {
  const actionResult = await callOperation('{= actionRoute =}', args)
  await resources.invalidateQueriesUsing(entitiesUsed)
  return actionResult
}

export const entitiesUsed = {=& entitiesArray =}

export default {= actionFnName =}


