{{={= =}=}}
import { useQuery } from '../queries'
import config from '../config.js'
import api, { setAuthToken, handleApiError } from '../api.js'

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

const use{= userEntity =} = () => {
  const {
    data: {= userEntityLower =},
    refetch: refetch{= userEntity =},
    isFetching: isFetching{= userEntity =}, 
  } = useQuery(getMe)

  return {
    {= userEntityLower =},
    refetch{= userEntity =},
    isFetching{= userEntity =}
  }
}

export default use{= userEntity =}


