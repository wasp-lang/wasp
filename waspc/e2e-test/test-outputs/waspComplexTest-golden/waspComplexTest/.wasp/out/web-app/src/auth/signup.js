import api, { handleApiError } from '../api.js'

export default async function signup(userFields) {
  try {
    await api.post('/auth/local/signup', userFields)
  } catch (error) {
    handleApiError(error)
  }
}
