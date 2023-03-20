{{={= =}=}}
import api, { handleApiError } from '../api'

export default async function signup(userFields) {
  try {
    await api.post('{= signupPath =}', userFields)
  } catch (error) {
    handleApiError(error)
  }
}
