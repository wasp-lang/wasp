import { clearLocalStorage } from '../api.js'
import queryCache from '../queryCache'

const logout = () => {
  clearLocalStorage()

  // TODO(matija): We are currently invalidating all the queries, but we should invalidate only the
  // non-public, user-dependent ones.
  queryCache.invalidateQueries()

  // TODO(matija): We are currently clearing all the queries, but we should clear only the
  // non-public, user-dependent ones.
  queryCache.clear()
}

export default logout

