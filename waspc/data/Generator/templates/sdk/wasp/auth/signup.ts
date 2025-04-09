{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'

export default async function signup(data: { username: string; password: string }): Promise<void> {
  try {
    await api.post('{= signupPath =}', data)
  } catch (error) {
    throw handleApiError(error)
  }
}
