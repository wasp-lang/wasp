{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { type UserUsernameAndPasswordSignupFields } from '../../providers/types'

export async function signup(data: { username: string; password: string } & UserUsernameAndPasswordSignupFields): Promise<void> {
  try {
    await api.post('{= signupPath =}', data)
  } catch (error) {
    throw handleApiError(error)
  }
}

