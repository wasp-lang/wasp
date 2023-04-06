import { useQuery } from '../queries'
import api, { handleApiError } from '../api'
import { HttpMethod } from '../types'
// todo(filip): turn into a proper import
import { type SanitizedUser as User } from '../../../server/src/_types/' 

export default function useAuth(queryFnArgs?: unknown, config?: any) {
  return useQuery(getMe, queryFnArgs, config)
}

export async function getMe(): Promise<User | null> {
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
