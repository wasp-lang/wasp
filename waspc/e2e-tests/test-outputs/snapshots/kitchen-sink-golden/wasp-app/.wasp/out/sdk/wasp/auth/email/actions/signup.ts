import { api } from 'wasp/client/api';
import { SuccessResponseSchema } from '../../responseSchemas';
import { type UserEmailSignupFields } from '../../providers'

type EmailSignupData = {
  email: string
  password: string
} & UserEmailSignupFields

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  const json = await api.post('/auth/email/signup', {
    json: data,
  }).json();
  const { success } = SuccessResponseSchema.parse(json);
  return { success };
}
