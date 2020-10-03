import { callOperation } from '../operations'

const createUser = async (args) => {
  return await callOperation('queries/create-user', args)
}

export default createUser
