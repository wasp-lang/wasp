import { api, handleApiError } from '../../../api/index.js'

// PRIVATE API
export type UsernameSignupData = {
  username: string
  password: string
}

// PUBLIC API
export async function signup(data: UsernameSignupData): Promise<void> {
  try {
    await api.post('/auth/username/signup', {
      json: data,
    })
  } catch (error) {
    throw handleApiError(error)
  }
}
