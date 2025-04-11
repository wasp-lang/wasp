{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { type UsernameAndPasswordUserSignupFieldType } from '../../providers/types'

export async function signup(data: { username: string; password: string } & UsernameAndPasswordUserSignupFieldType): Promise<void> {
  try {
    await api.post('{= signupPath =}', data)
  } catch (error) {
    throw handleApiError(error)
  }
}

