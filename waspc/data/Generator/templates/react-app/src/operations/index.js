import api, { handleApiError } from '../api.js'

export async function callOperation(operationRoute, args) {
  try {
    const response = await api.post(`/${operationRoute}`, args)
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}

