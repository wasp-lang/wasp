import { clearAuthToken } from '../api.js'
import queryCache from '../queryCache'

const logout = () => {
  console.log('going to log out!')

  // TODO(matija): delete whole local storage.

  clearAuthToken()
  queryCache.invalidateQueries('auth/me')
  queryCache.clear()
}

export default logout
