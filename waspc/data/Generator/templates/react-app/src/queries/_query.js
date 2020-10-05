{{={= =}=}}
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

const {= queryFnName =} = async (args) => {
  return await callOperation('{= queryRoute =}', args)
}

const queryCacheKey = '{= queryRoute =}'
{= queryFnName =}.queryCacheKey = queryCacheKey

export const entitiesUsed = {=& entitiesArray =}
resources.addResourcesUsedByQuery(queryCacheKey, entitiesUsed)

export default {= queryFnName =}

