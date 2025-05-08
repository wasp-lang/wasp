{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
{=# usernameAndPasswordUserSignupFields.isDefined =}
import { type UserUsernameAndPasswordSignupFields } from '../../providers/types'

interface UsernameSignupData extends UserUsernameAndPasswordSignupFields {}
{=/ usernameAndPasswordUserSignupFields.isDefined =}

interface UsernameSignupData {
  username: string
  password: string
}

// PUBLIC API
export async function signup(data: UsernameSignupData): Promise<void> {
  try {
    await api.post('{= signupPath =}', data)
  } catch (error) {
    throw handleApiError(error)
  }
}

