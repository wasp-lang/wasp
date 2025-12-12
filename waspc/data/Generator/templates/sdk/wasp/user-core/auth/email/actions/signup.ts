{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
{=# emailUserSignupFields.isDefined =}
import { type UserEmailSignupFields } from '../../providers'
{=/ emailUserSignupFields.isDefined =}

type EmailSignupData = {
  email: string
  password: string
}{=# emailUserSignupFields.isDefined =} & UserEmailSignupFields{=/ emailUserSignupFields.isDefined =}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const response = await api.post('{= signupPath =}', data);
    return response.data;
  } catch (e) {
    throw handleApiError(e);
  }
}
