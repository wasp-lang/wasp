import { api, handleApiError } from '../../../api/index.js';
import { SuccessResponseSchema } from '@wasp/lib-sdk-core';
import type { UserEmailSignupFields } from '../../providers'

// PRIVATE API
export type EmailSignupData = {
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
