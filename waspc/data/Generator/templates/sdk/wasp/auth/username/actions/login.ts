{{={= =}=}}
import { api } from 'wasp/client/api'
import { initSession } from '../../helpers/user'
import { SessionResponseSchema } from '../../responseSchemas'

export async function login(data: { username: string, password: string }): Promise<void> {
  const json = await api.post('{= loginPath =}', {
    json: data,
  }).json()
  const { sessionId } = SessionResponseSchema.parse(json)
  await initSession(sessionId)
}
