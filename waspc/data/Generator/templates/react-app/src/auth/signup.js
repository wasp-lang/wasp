import api, { handleApiError } from '../api'

export default async function signup(userFields) {
  try {
    await api.post('/auth/signup', userFields)
  } catch (error) {
    handleApiError(error)
  }
}
