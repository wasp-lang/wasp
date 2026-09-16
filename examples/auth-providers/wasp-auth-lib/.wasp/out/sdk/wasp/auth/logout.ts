import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import { api, getLastAuthProviderId, removeLocalUserData } from '../api/index.js'
import { clientAuthAdapters } from '../client/auth/providers.js'
import { invalidateAndRemoveQueries } from '../client/operations/internal/resources.js'

// PUBLIC API
export default async function logout(): Promise<void> {
  // Read before the server call: teardown below clears local storage.
  const sessionProviderId = getLastAuthProviderId()
  try {
    // Server first: dual sign-out needs the Wasp session header, and the
    // session row's recorded minting provider tells the server which
    // provider's own session to revoke alongside Wasp's.
    await api.post('/auth/logout')
    // Then the MINTING provider's adapter clears its own client-side state
    // (Clerk's signOut(), a token store's clear()), so that provider's browser
    // session ends too. Other providers' sessions are deliberately left alone:
    // they did not vouch for this login.
    if (sessionProviderId !== null) {
      // Widened lookup type: in an app without external providers the registry
      // is `{}` and indexing it types as `never`.
      const adapter = (
        clientAuthAdapters as Partial<Record<string, ClientAuthAdapter>>
      )[sessionProviderId]
      await adapter?.onLogout?.()
    }
  } finally {
    // Even if the logout request fails, we still want to remove the local user
    // data (the session id AND the last-provider marker, so nothing silently
    // resumes after an explicit logout) in case the logout failed because of a
    // network error and the user walked away from the computer.
    removeLocalUserData()

    // TODO(filip): We are currently invalidating and removing  all the queries, but
    // we should remove only the non-public, user-dependent ones.
    await invalidateAndRemoveQueries()
  }
}
