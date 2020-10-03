import { callOperation } from '../operations'

const getUser = async (args) => {
  return await callOperation('queries/get-user', args)
}

getUser.useQueryKey = 'queries/get-user'

export default getUser

