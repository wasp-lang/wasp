{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { initSession } from './helpers/user'

export default async function login(userFields: { username: string, password: string }): Promise<void> {
  try {
    const response = await api.post('{= loginPath =}', userFields)

    await initSession(response.data.sessionId)
  } catch (error) {
    throw handleApiError(error)
  }
}
