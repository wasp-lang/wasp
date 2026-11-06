import { api, handleApiError } from '../../../api/index.js'
import { SuccessResponseSchema } from '@wasp/lib-sdk-core'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean }> {
  try {
    const { success } = await api.post('/auth/email/verify-email', {
      json: data,
    }).json(SuccessResponseSchema)
    return { success }
  } catch (e) {
    throw handleApiError(e)
  }
}
