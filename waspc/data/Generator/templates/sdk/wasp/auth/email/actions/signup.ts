{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js';
import { SuccessResponseSchema } from '../../responseSchemas';
import { browserAppDelivery } from '../../../client/index.js';
{=# isEmailUserSignupFieldsDefined =}
import { type UserEmailSignupFields } from '../../providers'
{=/ isEmailUserSignupFieldsDefined =}

type EmailSignupData = {
  email: string
  password: string
}{=# isEmailUserSignupFieldsDefined =} & UserEmailSignupFields{=/ isEmailUserSignupFieldsDefined =}

// PUBLIC API
export async function signup(data: EmailSignupData): Promise<{ success: boolean }> {
  try {
    const { success } = await api.post(browserAppDelivery.waspApiPath('{= signupPath =}'), {
      json: data,
    }).json(SuccessResponseSchema);
    return { success };
  } catch (e) {
    throw handleApiError(e);
  }
}
