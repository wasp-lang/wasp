import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import type { AuthProviderId, ExternalAuthProviderId } from '../../auth/provider.js'
import {
  api,
  getLastAuthProviderId,
  getSessionId,
  removeLocalUserData,
  setSessionId,
} from '../../api/index.js'
import { invalidateAndRemoveQueries } from '../operations/internal/resources.js'
import { config } from '../config.js'
import { env } from '../env.js'
import { createClientAdapter as createWaspAuthClientAdapter } from '@wasp.sh/auth/client'

/**
 * The client halves of the app's auth providers, instantiated from each
 * adapter package's client entry with the same runtime-window discipline as
 * the server halves: an adapter sees only what Wasp hands it here. Keyed by
 * provider id; providers without a client package simply have no entry.
 */

// Each adapter's env is narrowed to exactly the vars its manifest declared:
// what an adapter reads is what its manifest shows. The session sink is
// pre-bound to the provider id, so an adapter cannot write another provider's
// resume/logout marker.
function makeClientRuntime(
  providerId: AuthProviderId,
  declaredClientEnvVarNames: readonly string[],
) {
  return {
    apiUrl: config.apiUrl,
    env: Object.fromEntries(
      declaredClientEnvVarNames.map((name) => [
        name,
        (env as Record<string, string | undefined>)[name],
      ]),
    ),
    setSession: async (sessionId: string): Promise<void> => {
      setSessionId(sessionId, providerId)
      await invalidateAndRemoveQueries()
    },
  }
}

// Wasp's own auth, instantiated from the @wasp.sh/auth lib exactly like an
// adapter package's client entry: its forms and actions read this runtime.
createWaspAuthClientAdapter(makeClientRuntime('wasp', []), {"clientOAuthCallbackPath":"/oauth/callback","methods":{"discord":{"requiredScopes":["identify"]},"email":{"emailVerificationClientRoute":"/email-verification-","fromField":{"email":"kitchen-sink@wasp.sh","name":"Wasp Kitchen Sink"},"passwordResetClientRoute":"/password-reset"},"github":{"requiredScopes":[]},"google":{"requiredScopes":["profile"]},"microsoft":{"requiredScopes":["openid","profile","email"]},"slack":{"requiredScopes":["openid"]}},"onAuthSucceededRedirectTo":"/"})

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

