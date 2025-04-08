{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
import { type UserSignupFieldsData } from './providers/types'

export default async function signup(userFields: { username: string; password: string } & UserSignupFieldsData): Promise<void> {
  try {
    await api.post('{= signupPath =}', userFields)
  } catch (error) {
    throw handleApiError(error)
  }
}
