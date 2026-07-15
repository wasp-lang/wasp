{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js'
{=# usernameAndPasswordUserSignupFields.isDefined =}
import { type UserUsernameAndPasswordSignupFields } from '../../providers'
{=/ hasUsernameAndPasswordUserSignupFields =}

type UsernameSignupData = {
  username: string
  password: string
}{=# hasUsernameAndPasswordUserSignupFields =} & UserUsernameAndPasswordSignupFields{=/ hasUsernameAndPasswordUserSignupFields =}

// PUBLIC API
export async function signup(data: UsernameSignupData): Promise<void> {
  try {
    await api.post('{= signupPath =}', {
      json: data,
    })
  } catch (error) {
    throw handleApiError(error)
  }
}
