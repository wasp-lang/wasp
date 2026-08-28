import { api, handleApiError } from '../../../api/index.js'
import { initSession } from '../../helpers/user'
import { SessionResponseSchema } from '../../responseSchemas'

export async function login(data: { username: string, password: string }): Promise<void> {
  try {
    const { sessionId } = await api.post('/auth/username/login', {
      json: data,
    }).json(SessionResponseSchema)
    await initSession(sessionId)
  } catch (error) {
    throw handleApiError(error)
  }
}
