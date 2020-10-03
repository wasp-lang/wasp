import { callOperation } from '../operations'

const getUsers = async (args) => {
  return await callOperation('queries/get-users', args)
}

getUsers.useQueryKey = 'queries/get-users'

export default getUsers

