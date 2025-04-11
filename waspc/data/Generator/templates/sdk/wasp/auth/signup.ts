{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { type UsernameAndPasswordUserSignupFieldType } from './providers/types'

export default async function signup(userFields: { username: string; password: string } & UsernameAndPasswordUserSignupFieldType): Promise<void> {
  try {
    await api.post('{= signupPath =}', userFields)
  } catch (error) {
    throw handleApiError(error)
  }
}
