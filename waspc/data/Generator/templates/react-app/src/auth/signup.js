import config from '../config.js'
import api, { handleApiError } from '../api.js'
import login from './login.js'

export default async function signup(userFields) {
  try {
    await api.post(config.apiUrl + '/auth/signup', userFields)
    await login(userFields.username, userFields.password)
  } catch (error) {
    handleApiError(error)
  }
}
