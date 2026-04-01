import { api, removeLocalUserData } from 'wasp/client/api'
import { invalidateAndRemoveQueries } from '../client/operations/internal/resources.js'

// PUBLIC API
export default async function logout(): Promise<void> {
  try {
    await api.post('/auth/logout')
  } finally {
    // Even if the logout request fails, we still want to remove the local user data
    // in case the logout failed because of a network error and the user walked away
    // from the computer.
    removeLocalUserData()

    // TODO(filip): We are currently invalidating and removing  all the queries, but
    // we should remove only the non-public, user-dependent ones.
    await invalidateAndRemoveQueries()
  }
}
