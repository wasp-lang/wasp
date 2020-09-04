{{={= =}=}}
import { callOperation } from '../operations'

const {= queryFnName =} = async (args) => {
  return await callOperation('{= queryRoute =}', args)
}

{= queryFnName =}.useQueryKey = '{= queryRoute =}'

export default {= queryFnName =}

