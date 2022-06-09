import { invalidateAndRemoveQueries } from '../operations/resources'
import config from '../config.js'
import api from '../api.js'

export default async function logout() {
  await api.post(config.apiUrl + '/auth/logout')

  // TODO(filip): We are currently invalidating and removing  all the queries, but
  // we should remove only the non-public, user-dependent ones.
  await invalidateAndRemoveQueries()
}
