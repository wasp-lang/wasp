import { useQuery } from '../queries'
import api, { handleApiError } from '../api.js'

export default function useAuth(queryFnArgs, config) {
  return useQuery(getMe, queryFnArgs, config)
}
async function getMe() {
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
