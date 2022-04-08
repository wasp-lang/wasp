import { useQuery } from '../queries'
import config from '../config.js'
import api, { handleApiError } from '../api.js'

const getMe = async () => {
  try {
    const response = await api.get(config.apiUrl + '/auth/me')

    return response.data
  } catch (error) {
    if (error.response?.status === 403) {
      return null
    } else {
      handleApiError(error)
    }
  }
}
getMe.queryCacheKey = 'auth/me'

const useAuth = (queryFnArgs, config) => {
  return useQuery(getMe, queryFnArgs, config)
}

export default useAuth
