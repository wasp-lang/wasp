{{={= =}=}}
import { api } from 'wasp/client/api'
{=# usernameAndPasswordUserSignupFields.isDefined =}
import { type UserUsernameAndPasswordSignupFields } from '../../providers'
{=/ usernameAndPasswordUserSignupFields.isDefined =}

type UsernameSignupData = {
  username: string
  password: string
}{=# usernameAndPasswordUserSignupFields.isDefined =} & UserUsernameAndPasswordSignupFields{=/ usernameAndPasswordUserSignupFields.isDefined =}

// PUBLIC API
export async function signup(data: UsernameSignupData): Promise<void> {
  await api.post('{= signupPath =}'.slice(1), {
    json: data,
  })
}
