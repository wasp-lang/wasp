import config from '../config.js'
import queryCache from '../queryCache'
import api, { setAuthToken, handleApiError } from '../api.js'

const login = async (email, password) => {
  try {
    const args = { email, password }
    const response = await api.post(config.apiUrl + '/auth/login', args)

    setAuthToken(response.data.token)

    // TODO(matija): We should invalidate only non-public queries.
    queryCache.invalidateQueries()
  } catch (error) {
    handleApiError(error)
  }
}

export default login

