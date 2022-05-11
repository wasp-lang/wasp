import config from '../config.js'
import { removePrivateQueries } from '../operations/resources'
import api, { setAuthToken, handleApiError } from '../api.js'

export default async function login(email, password) {
  try {
    const args = { email, password }
    const response = await api.post(config.apiUrl + '/auth/login', args)

    setAuthToken(response.data.token)
    removePrivateQueries()

  } catch (error) {
    handleApiError(error)
  }
}