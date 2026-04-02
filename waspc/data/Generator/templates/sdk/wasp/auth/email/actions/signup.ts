{{={= =}=}}
import { api } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';
{=# emailUserSignupFields.isDefined =}
import { type UserEmailSignupFields } from '../../providers'
{=/ emailUserSignupFields.isDefined =}

type EmailSignupData = {
  email: string
  password: string
}{=# emailUserSignupFields.isDefined =} & UserEmailSignupFields{=/ emailUserSignupFields.isDefined =}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  const { success } = await api.post('{= signupPath =}', {
    json: data,
  }).json(SuccessResponseSchema);
  return { success };
}
