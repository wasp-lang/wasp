import { api } from 'wasp/client/api'
import { SuccessResponseSchema } from '../../responseSchemas'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  const json = await api.post('/auth/email/verify-email', {
    json: data,
  }).json()
  const { success, reason } = SuccessResponseSchema.parse(json)
  return { success, reason }
}
