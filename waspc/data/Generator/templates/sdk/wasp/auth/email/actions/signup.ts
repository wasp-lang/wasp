{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api';
{=# emailUserSignupFields.isDefined =}
import { type UserEmailSignupFields } from '../../providers/types'

interface EmailSignupData extends UserEmailSignupFields {}
{=/ emailUserSignupFields.isDefined =}

interface EmailSignupData {
  username: string
  password: string
}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const response = await api.post('{= signupPath =}', data);
    return response.data;
  } catch (e) {
    throw handleApiError(e);
  }
}
