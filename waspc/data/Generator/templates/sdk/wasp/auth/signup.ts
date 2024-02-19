{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'

export default async function signup(userFields: { username: string; password: string }): Promise<void> {
  try {
    await api.post('{= signupPath =}', userFields)
  } catch (error) {
    handleApiError(error)
  }
}
