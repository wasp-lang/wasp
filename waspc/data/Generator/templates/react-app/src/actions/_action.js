{{={= =}=}}
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

const {= actionFnName =} = async (args) => {
  const actionResult = await callOperation('{= actionRoute =}', args)
  resources.invalidateQueriesUsing(entitiesUsed)
  return actionResult
}

export const entitiesUsed = {=& entitiesArray =}

export default {= actionFnName =}


