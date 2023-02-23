import api, { handleApiError } from '../api.js'
import { setupAuth } from './helpers/user'

export default async function login(username, password) {
  try {
    const args = { username, password }
    const response = await api.post('/auth/login', args)

    await setupAuth({ token: response.data.token })
  } catch (error) {
    handleApiError(error)
  }
}
