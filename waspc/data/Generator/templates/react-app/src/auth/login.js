{{={= =}=}}
import api, { handleApiError } from '../api'
import { initSession } from './helpers/user'

export default async function login(username, password) {
  try {
    const args = { username, password }
    const response = await api.post('{= loginPath =}', args)

    await initSession(response.data.token)
  } catch (error) {
    handleApiError(error)
  }
}
