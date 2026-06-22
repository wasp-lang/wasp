import { api, handleApiError } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';
import { type UserEmailSignupFields } from '../../providers'

type EmailSignupData = {
  email: string
  password: string
} & UserEmailSignupFields

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const { success } = await api.post('/auth/email/signup', {
      json: data,
    }).json(SuccessResponseSchema);
    return { success };
  } catch (e) {
    throw handleApiError(e);
  }
}
