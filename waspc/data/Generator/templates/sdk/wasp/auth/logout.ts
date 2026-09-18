import type { ClientAuthHandler } from '@wasp.sh/auth-contract/client'
import { api, getLastAuthScheme, removeLocalUserData } from '../api/index.js'
import { clientAuthHandlers } from '../client/auth/schemes.js'
import { invalidateAndRemoveQueries } from '../client/operations/internal/resources.js'

// PUBLIC API
export default async function logout(): Promise<void> {
  // Read before the server call: teardown below clears local storage.
  const lastScheme = getLastAuthScheme()
  try {
    // Server first: the scheme that authenticated the request invalidates
    // the credential it carries (a session row, a cookie).
    await api.post('/auth/logout')
    // Then the handler of the scheme that signed in clears its own
    // client-side state (Clerk's signOut(), a token store's clear()).
    if (lastScheme !== null) {
      const clientAuthHandler = (
        clientAuthHandlers as Partial<Record<string, ClientAuthHandler>>
      )[lastScheme]
      await clientAuthHandler?.onLogout?.()
    }
  } finally {
    // Even if the logout request fails, we still want to remove the local
    // credential in case the logout failed because of a network error and
    // the user walked away from the computer.
    removeLocalUserData()

    // TODO(filip): We are currently invalidating and removing  all the queries, but
    // we should remove only the non-public, user-dependent ones.
    await invalidateAndRemoveQueries()
  }
}
