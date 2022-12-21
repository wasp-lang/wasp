import api, { handleApiError } from '../api.js'

export async function callAction(actionRoute, args) {
  try {
    const response = await api.post(`/${actionRoute}`, args)
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}

export async function callQuery(queryRoute, args) {
  try {
    // TODO: This is to help us preserve the types of the args.
    // In future, we should get type info from users via wasp file.
    const response = await api.get(`/${queryRoute}`, { params: { args: JSON.stringify(args) } })
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}
