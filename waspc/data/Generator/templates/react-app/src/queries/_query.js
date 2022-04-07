{{={= =}=}}
import { callOperation } from '../operations'
import * as resources from '../operations/resources'

function {= queryFnName =}(args) {
  return callOperation('{= queryRoute =}', args)
}

const queryCacheKey = '{= queryRoute =}'
{= queryFnName =}.queryCacheKey = queryCacheKey

const entitiesUsed = {=& entitiesArray =}
resources.addResourcesUsedByQuery(queryCacheKey, entitiesUsed)

export default {= queryFnName =}

