{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
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
    const response = await api.post('{= signupPath =}', data);
    return response.data;
  } catch (e) {
    throw handleApiError(e);
  }
}
