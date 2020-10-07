import { useQuery } from '../queries'
import config from '../config.js'
import api, { setAuthToken } from '../api.js'

const getMe = async () => {
  try {
    const response = await api.get(config.apiUrl + '/auth/me')

    return response.data
  } catch (error) {
    // TODO(matija): refine this, extract maybe from callOperation
    console.log('error while fetching!')
    console.log(error)

    // Check if error is 401.
    return null

    throw error
  }
}
getMe.useQueryKey = 'auth/me'

const useMe = () => {
  const { data: me, refetch: refetchMe, isFetching: isFetchingMe } = useQuery(getMe)

  return { me, refetchMe, isFetchingMe }
}

export default useMe

