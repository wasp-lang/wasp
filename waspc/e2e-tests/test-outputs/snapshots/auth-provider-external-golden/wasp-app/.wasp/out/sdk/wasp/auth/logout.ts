import { api, removeLocalUserData } from '../api/index.js'
import { clientAuthAdapter } from '../client/auth/provider.js'
import { invalidateAndRemoveQueries } from '../client/operations/internal/resources.js'

// PUBLIC API
export default async function logout(): Promise<void> {
  try {
    // The adapter clears its own client-side state first (Clerk's signOut(),
    // a token store's clear()), then Wasp revokes the session server-side.
    await clientAuthAdapter.onLogout?.()
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
