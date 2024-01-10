import api, { removeLocalUserData } from '../api'
import { invalidateAndRemoveQueries } from '../operations/resources'

export default async function logout(): Promise<void> {
  await api.post('/auth/logout')
 
  removeLocalUserData()
  // TODO(filip): We are currently invalidating and removing  all the queries, but
  // we should remove only the non-public, user-dependent ones.
  await invalidateAndRemoveQueries()
}
