import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import type { ExternalAuthProviderId } from '../../auth/provider.js'
import {
  api,
  getLastAuthProviderId,
  getSessionId,
  removeLocalUserData,
} from '../../api/index.js'

/**
 * The client halves of the app's auth providers, instantiated from each
 * adapter package's client entry with the same runtime-window discipline as
 * the server halves: an adapter sees only what Wasp hands it here. Keyed by
 * provider id; providers without a client package simply have no entry.
 */


// PRIVATE API
export const clientAuthAdapters: Partial<Record<ExternalAuthProviderId, ClientAuthAdapter>> = {
}

// PUBLIC API
/**
 * Attempts to silently re-establish a Wasp session from the provider of the
 * last login in this browser (ASP.NET's silent challenge, done in-page).
 *
 * Consults exactly ONE adapter -- the one recorded at the last session mint --
 * and never probes the others, so two live provider credentials can never
 * race, and an explicit logout (which clears the marker) can never be undone
 * by resume. Runs at the auth gate (`createAuthRequiredPage`) when an
 * authRequired page finds no user; login pages that want the
 * instant-completion path may call it eagerly too.
 *
 * Resolves to whether a session now exists. Single-flighted: concurrent
 * callers share one attempt.
 */
export function resumeSession(): Promise<boolean> {
  // No provider brings a client-side credential source, so there is nothing
  // to resume from; a live session is the only way to be authenticated.
  return Promise.resolve(getSessionId() !== null)
}


// PUBLIC API
/**
 * Logs in through the named provider's client adapter: pulls its current
 * credential and exchanges it for a Wasp session. The provider's own sign-in
 * flow must have completed first (or its credential must otherwise be live).
 * For providers without a client adapter, obtain the credential yourself and
 * call `exchangeCredentialForSession`.
 */
export async function loginWithAuthProvider(providerId: ExternalAuthProviderId): Promise<void> {
  throw new Error(
    `Auth provider '${providerId}' has no client-side credential source; ` +
      `obtain the credential yourself and call exchangeCredentialForSession().`,
  )
}

