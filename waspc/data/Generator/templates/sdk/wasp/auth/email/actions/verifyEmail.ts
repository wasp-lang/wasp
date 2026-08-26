{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js'
import { SuccessResponseSchema } from '../../responseSchemas'
import { browserAppDelivery } from '../../../client/index.js'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  try {
    const { success, reason } = await api.post(browserAppDelivery.waspApiPath('{= verifyEmailPath =}'), {
      json: data,
    }).json(SuccessResponseSchema)
    return { success, reason }
  } catch (e) {
    throw handleApiError(e)
  }
}
