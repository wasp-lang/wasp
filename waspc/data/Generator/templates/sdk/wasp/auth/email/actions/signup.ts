{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';
{=# hasEmailUserSignupFields =}
import { type UserEmailSignupFields } from '../../providers'
{=/ hasEmailUserSignupFields =}

type EmailSignupData = {
  email: string
  password: string
}{=# hasEmailUserSignupFields =} & UserEmailSignupFields{=/ hasEmailUserSignupFields =}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const { success } = await api.post('{= signupPath =}', {
      json: data,
    }).json(SuccessResponseSchema);
    return { success };
  } catch (e) {
    throw handleApiError(e);
  }
}
