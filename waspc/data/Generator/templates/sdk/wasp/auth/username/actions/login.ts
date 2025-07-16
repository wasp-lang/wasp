{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { initSession } from '../../helpers/user'

export async function login(data: { username: string, password: string }): Promise<void> {
  try {
    const response = await api.post('{= loginPath =}', data)

    await initSession(response.data.sessionId)
  } catch (error) {
    throw handleApiError(error)
  }
}
