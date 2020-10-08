import { clearLocalStorage } from '../api.js'
import queryCache from '../queryCache'

const logout = () => {
  clearLocalStorage()

  // TODO(matija): We should also invalidate other non-public queries.
  queryCache.invalidateQueries('auth/me')

  // TODO(matija): We should clear only non-public queries, no need
  // to clear everything.
  queryCache.clear()
}

export default logout

