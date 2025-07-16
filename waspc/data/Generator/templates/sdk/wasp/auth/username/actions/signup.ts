{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'
{=# usernameAndPasswordUserSignupFields.isDefined =}
import { type UserUsernameAndPasswordSignupFields } from '../../providers'
{=/ usernameAndPasswordUserSignupFields.isDefined =}

type UsernameSignupData = {
  username: string
  password: string
}{=# usernameAndPasswordUserSignupFields.isDefined =} & UserUsernameAndPasswordSignupFields{=/ usernameAndPasswordUserSignupFields.isDefined =}

// PUBLIC API
export async function signup(data: UsernameSignupData): Promise<void> {
  try {
    await api.post('{= signupPath =}', data)
  } catch (error) {
    throw handleApiError(error)
  }
}

