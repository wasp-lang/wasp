import { useQuery } from '../queries'
import api, { handleApiError } from '../api'
import { HttpMethod } from '../types'
// todo(filip): turn into a proper import
import { type SanitizedUser } from '../../../server/src/_types/' 

export default function useAuth() {
  return useQuery(getMe)
}

export async function getMe(): Promise<SanitizedUser | null> {
  try {
    const response = await api.get('/auth/me')

    return response.data
  } catch (error) {
    if (error.response?.status === 401) {
      return null
    } else {
      handleApiError(error)
    }
  }
}

getMe.queryCacheKey = ['auth/me']
getMe.route = { method: HttpMethod.Get, path: '/auth/me' }
