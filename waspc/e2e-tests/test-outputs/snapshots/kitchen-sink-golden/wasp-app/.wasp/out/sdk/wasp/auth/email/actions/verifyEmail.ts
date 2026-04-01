import { api } from 'wasp/client/api'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  return api.post('/auth/email/verify-email'.slice(1), {
    json: data,
  }).json()
}
