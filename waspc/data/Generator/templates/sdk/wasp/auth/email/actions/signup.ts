{{={= =}=}}
import { api } from 'wasp/client/api';
{=# emailUserSignupFields.isDefined =}
import { type UserEmailSignupFields } from '../../providers'
{=/ emailUserSignupFields.isDefined =}

type EmailSignupData = {
  email: string
  password: string
}{=# emailUserSignupFields.isDefined =} & UserEmailSignupFields{=/ emailUserSignupFields.isDefined =}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  return api.post('{= signupPath =}'.slice(1), {
    json: data,
  }).json();
}
