import { api } from 'wasp/client/api'
import { SuccessResponseSchema } from '../../responseSchemas'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  const { success, reason } = await api.post('/auth/email/verify-email', {
    json: data,
  }).json(SuccessResponseSchema)
  return { success, reason }
}
