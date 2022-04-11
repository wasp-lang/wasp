import { clearLocalStorage } from '../api.js'
import { queryClient } from '../queryClient'

const logout = () => {
  clearLocalStorage()

  // TODO(matija): We are currently invalidating all the queries, but we should invalidate only the
  // non-public, user-dependent ones.
  queryClient.invalidateQueries()

  // TODO(matija): We are currently clearing all the queries, but we should clear only the
  // non-public, user-dependent ones.
  queryClient.clear()
}

export default logout

