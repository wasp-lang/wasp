import { clearAuthToken } from '../api.js'
import queryCache from '../queryCache'

const logout = () => {
  console.log('going to log out!')

  // TODO(matija): delete whole local storage.

  clearAuthToken()
  // Should also invalidate non-public queries.
  queryCache.invalidateQueries('auth/me')
  // Clear only non-public queries.
  queryCache.clear()
}

export default logout
