import api, { handleApiError } from '../api.js'
import config from '../config.js'

export async function callOperation(operationRoute, args) {
  try {
    const response = await api.post(config.apiUrl + '/' + operationRoute, args)
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}

