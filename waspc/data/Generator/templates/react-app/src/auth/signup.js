import config from '../config.js'
import api, { handleApiError } from '../api.js'


const signup = async (userFields) => {
  try {
    await api.post(config.apiUrl + '/auth/signup', userFields)
  } catch (error) {
    handleApiError(error)
  }
}

export default signup
