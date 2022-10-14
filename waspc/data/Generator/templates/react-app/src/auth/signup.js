import config from '../config.js'
import api, { handleApiError } from '../api.js'

export default async function signup(userFields) {
  try {
    await api.post('/auth/signup', userFields)
  } catch (error) {
    handleApiError(error)
  }
}
