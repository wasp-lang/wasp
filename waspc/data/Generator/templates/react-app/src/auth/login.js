import config from '../config.js'
import queryCache from '../queryCache'
import api, { setAuthToken, handleApiError } from '../api.js'

const login = async (email, password) => {
  try {
    const args = { email, password }
    const response = await api.post(config.apiUrl + '/auth/login', args)

    setAuthToken(response.data.token)

    // TODO(matija): Currently we are removing all the queries, but we should remove only
    // non-public, user-dependent queries - public queries are expected not to change in respect
    // to the currently logged in user.
    queryCache.removeQueries()

  } catch (error) {
    handleApiError(error)
  }
}

export default login
