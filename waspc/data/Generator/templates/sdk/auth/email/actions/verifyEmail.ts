{{={= =}=}}
import { api, handleApiError } from 'wasp/client/api'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  try {
    const response = await api.post('{= verifyEmailPath =}', data)
    return response.data
  } catch (e) {
    handleApiError(e)
  }
}
