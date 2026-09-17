import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import type { AuthSchemeName } from '../../auth/scheme.js'
import {
  getRequestCredential,
  registerCredentialSource,
  setCredential,
} from '../../api/index.js'
import { invalidateAndRemoveQueries, invalidateQueryByKey } from '../operations/internal/resources.js'
import { config } from '../config.js'
import { env } from '../env.js'
import { createClientAdapter as createClientAdapter_0 } from '@wasp.sh/auth/client'
import { createClientAdapter as createClientAdapter_1 } from '@wasp.sh/auth-clerk/client'

/**
 * The client halves of the app's auth schemes, instantiated from each
 * handler package's client entry with the same runtime-window discipline as
 * the server halves: an adapter sees only what Wasp hands it here. Keyed by
 * scheme name; schemes without a client package simply have no entry.
 */

// PRIVATE API
export const defaultScheme: AuthSchemeName = 'wasp'

// Each adapter's env is narrowed to exactly the vars its manifest declared:
// what an adapter reads is what its manifest shows. The credential sink is
// pre-bound to the scheme name, so an adapter cannot write another scheme's
// logout marker.
function makeClientRuntime(
  scheme: AuthSchemeName,
  declaredClientEnvVarNames: readonly string[],
) {
  const mountUrl = `${config.apiUrl}/auth/${scheme}`
  return {
    scheme,
    apiUrl: config.apiUrl,
    mountUrl,
    // The user's credential is spent only on this scheme's own routes: the
    // adapter never sees it, and cannot aim it at other routes or origins.
    fetch: async (input: string | URL, init?: RequestInit): Promise<Response> => {
      const url = new URL(String(input), window.location.href)
      const mount = new URL(mountUrl, window.location.href)
      const isUnderMount =
        url.origin === mount.origin &&
        (url.pathname === mount.pathname || url.pathname.startsWith(`${mount.pathname}/`))
      if (!isUnderMount) {
        throw new Error(
          `The client adapter of auth scheme '${scheme}' tried an authenticated request to '${url.href}', outside its own routes (${mountUrl}).`,
        )
      }
      const headers = new Headers(init?.headers)
      const credential = await getRequestCredential()
      if (credential !== null) {
        headers.set('Authorization', `Bearer ${credential}`)
      }
      return fetch(url, { ...init, headers, credentials: 'include' })
    },
    // Only the current user: a linked account does not change anything else.
    refreshUser: (): Promise<void> => invalidateQueryByKey(['auth/me']),
    env: Object.fromEntries(
      declaredClientEnvVarNames.map((name) => [
        name,
        (env as Record<string, string | undefined>)[name],
      ]),
    ),
    setCredential: async (
      credential: string | null,
      options?: { persistent?: boolean },
    ): Promise<void> => {
      setCredential(credential, scheme, options)
      await invalidateAndRemoveQueries()
    },
  }
}

// PRIVATE API
export const clientAuthAdapters: Partial<Record<AuthSchemeName, ClientAuthAdapter>> = {
  'wasp': createClientAdapter_0(makeClientRuntime('wasp', []), {"onAuthSucceededRedirectTo":"/","clientOAuthCallbackPath":"/oauth/callback","methods":{"usernameAndPassword":{}}}),
  'clerk': createClientAdapter_1(makeClientRuntime('clerk', ['REACT_APP_CLERK_PUBLISHABLE_KEY']), undefined),
}

// The adapters' own credentials are the request path's fallback source:
// when no Wasp-issued credential is stored, the first adapter (default
// scheme first, then declaration order) that has a credential of its own
// (a hosted provider's token, say) puts it on the request. A stored Wasp
// credential always wins, so two live credentials never race.
const adaptersWithCredentials = [
  defaultScheme,
  ...(Object.keys(clientAuthAdapters) as AuthSchemeName[]).filter((name) => name !== defaultScheme),
]
  .map((name) => (clientAuthAdapters as Partial<Record<string, ClientAuthAdapter>>)[name])
  .filter((adapter): adapter is ClientAuthAdapter => adapter?.getCredential !== undefined)
if (adaptersWithCredentials.length > 0) {
  registerCredentialSource(async () => {
    for (const adapter of adaptersWithCredentials) {
      const credential = await adapter.getCredential!()
      if (credential !== null) {
        return credential
      }
    }
    return null
  })
}

// A login or logout inside a provider's own component (Clerk's widget)
// changes who the requests are for: refresh the cached queries.
for (const adapter of Object.values(clientAuthAdapters)) {
  adapter?.onCredentialChange?.(() => {
    void invalidateAndRemoveQueries()
  })
}
