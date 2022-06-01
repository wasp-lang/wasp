import { clearLocalStorage } from '../api.js'
import { invalidateAndRemoveQueries } from '../operations/resources'

export default async function logout() {
  clearLocalStorage()
  // TODO(filip): We are currently invalidating and removing  all the queries, but
  // we should remove only the non-public, user-dependent ones.
  await invalidateAndRemoveQueries()
}
