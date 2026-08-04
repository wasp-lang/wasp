{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js'
{=# isUsernameAndPasswordUserSignupFieldsDefined =}
import { type UserUsernameAndPasswordSignupFields } from '../../providers'
{=/ isUsernameAndPasswordUserSignupFieldsDefined =}

type UsernameSignupData = {
  username: string
  password: string
}{=# isUsernameAndPasswordUserSignupFieldsDefined =} & UserUsernameAndPasswordSignupFields{=/ isUsernameAndPasswordUserSignupFieldsDefined =}

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
