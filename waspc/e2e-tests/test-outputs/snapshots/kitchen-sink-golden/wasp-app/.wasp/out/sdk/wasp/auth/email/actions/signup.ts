import { api } from 'wasp/client/api';
import { type UserEmailSignupFields } from '../../providers'

type EmailSignupData = {
  email: string
  password: string
} & UserEmailSignupFields

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  return api.post('/auth/email/signup'.slice(1), {
    json: data,
  }).json();
}
