import { api, handleApiError } from '../../../api/index.js';
import { SuccessResponseSchema } from '../../responseSchemas';
import { browserAppDelivery } from '../../../client/index.js';
import { type UserEmailSignupFields } from '../../providers'

type EmailSignupData = {
  email: string
  password: string
} & UserEmailSignupFields

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const { success } = await api.post(browserAppDelivery.waspApiPath('/auth/email/signup'), {
      json: data,
    }).json(SuccessResponseSchema);
    return { success };
  } catch (e) {
    throw handleApiError(e);
  }
}
