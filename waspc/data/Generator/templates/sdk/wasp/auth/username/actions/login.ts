{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js'
import { initSession } from '@wasp.sh/lib-sdk-core/browser'
import { SessionResponseSchema } from '@wasp.sh/lib-sdk-core'

export async function login(data: { username: string, password: string }): Promise<void> {
  try {
    const { sessionId } = await api.post('{= loginPath =}', {
      json: data,
    }).json(SessionResponseSchema)
    await initSession(sessionId)
  } catch (error) {
    throw handleApiError(error)
  }
}
