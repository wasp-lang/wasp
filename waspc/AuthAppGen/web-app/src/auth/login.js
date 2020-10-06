import config from '../config.js'
import queryCache from '../queryCache'
import api, { setAuthToken } from '../api.js'

const login = async (email, password) => {
  try {
    const args = { email, password }
    const response = await api.post(config.apiUrl + '/auth/login', args)

    setAuthToken(response.data.token)
    queryCache.invalidateQueries()
  } catch (error) {
    // TODO(matija): refine this, extract maybe from callOperation
    console.log(error)
    throw error
  }
}

export default login
