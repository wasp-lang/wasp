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
    const response = await api.get(`/${queryRoute}`, { params: args })
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}
