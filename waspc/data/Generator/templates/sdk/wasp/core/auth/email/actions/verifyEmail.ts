{{={= =}=}}
import { api, handleApiError } from '../../../api/index.js'

// PUBLIC API
export async function verifyEmail(data: {
  token: string
}): Promise<{ success: boolean; reason?: string }> {
  try {
    const response = await api.post('{= verifyEmailPath =}', data)
    return response.data
  } catch (e) {
    throw handleApiError(e)
  }
}
