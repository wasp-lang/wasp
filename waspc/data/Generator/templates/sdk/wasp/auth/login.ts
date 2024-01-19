import api, { handleApiError } from 'wasp/api'
import { initSession } from './helpers/user'

export default async function login(username: string, password: string): Promise<void> {
  try {
    const args = { username, password }
    const response = await api.post('/auth/username/login', args)

    await initSession(response.data.sessionId)
  } catch (error) {
    handleApiError(error)
  }
}
