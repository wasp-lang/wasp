import { api, handleApiError } from 'wasp/client/api'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  try {
    const response = await api.post('/auth/email/verify-email', data)
    return response.data
  } catch (e) {
    throw handleApiError(e)
  }
}
