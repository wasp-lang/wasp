import type { ClientAuthHandler } from '@wasp.sh/auth-contract/client'
import type { AuthSchemeName } from '../../auth/scheme.js'
import { joinHandlerSpec } from '../../auth/handlerSpec.js'
import {
  getRequestCredential,
  registerCredentialSource,
  setCredential,
} from '../../api/index.js'
import { invalidateAndRemoveQueries, invalidateQueryByKey } from '../operations/internal/resources.js'
import { config } from '../config.js'
import { env } from '../env.js'
import { createClientAuthHandler as createClientAuthHandler_0 } from '@wasp.sh/auth/client'

/**
 * The client halves of the app's auth schemes, instantiated from each
 * handler package's client entry with the same runtime-window discipline as
 * the server halves: a handler sees only what Wasp hands it here. Keyed by
 * scheme name; schemes without a client package simply have no entry.
 */

// PRIVATE API
export const defaultScheme: AuthSchemeName = 'wasp'

// Each handler's env is narrowed to exactly the vars its manifest declared:
// what a handler reads is what its manifest shows. The credential sink is
// pre-bound to the scheme name, so a handler cannot write another scheme's
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
    // handler never sees it, and cannot aim it at other routes or origins.
    fetch: async (input: string | URL, init?: RequestInit): Promise<Response> => {
      const url = new URL(String(input), window.location.href)
      const mount = new URL(mountUrl, window.location.href)
      const isUnderMount =
        url.origin === mount.origin &&
        (url.pathname === mount.pathname || url.pathname.startsWith(`${mount.pathname}/`))
      if (!isUnderMount) {
        throw new Error(
          `The client auth handler of auth scheme '${scheme}' tried an authenticated request to '${url.href}', outside its own routes (${mountUrl}).`,
        )
      }
      const headers = new Headers(init?.headers)
      const credential = await getRequestCredential()
      if (credential !== null) {
        headers.set('Authorization', `Bearer ${credential}`)
      }
      return fetch(url, { ...init, headers })
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
export const clientAuthHandlers: Partial<Record<AuthSchemeName, ClientAuthHandler>> = {
  'wasp': createClientAuthHandler_0(makeClientRuntime('wasp', []) as unknown as Parameters<typeof createClientAuthHandler_0>[0], joinHandlerSpec({"onAuthSucceededRedirectTo":"/","clientOAuthCallbackPath":"/oauth/callback","methods":{"email":{},"google":{},"github":{},"slack":{},"discord":{},"microsoft":{}}}, []) as Parameters<typeof createClientAuthHandler_0>[1]),
}

// The handlers' own credentials are the request path's fallback source:
// when no Wasp-issued credential is stored, the first handler (default
// scheme first, then declaration order) that has a credential of its own
// (a hosted provider's token, say) puts it on the request. A stored Wasp
// credential always wins, so two live credentials never race.
const clientAuthHandlersWithCredentials = [
  defaultScheme,
  ...(Object.keys(clientAuthHandlers) as AuthSchemeName[]).filter((name) => name !== defaultScheme),
]
  .map((name) => (clientAuthHandlers as Partial<Record<string, ClientAuthHandler>>)[name])
  .filter((clientAuthHandler): clientAuthHandler is ClientAuthHandler => clientAuthHandler?.getCredential !== undefined)
if (clientAuthHandlersWithCredentials.length > 0) {
  registerCredentialSource(async () => {
    for (const clientAuthHandler of clientAuthHandlersWithCredentials) {
      const credential = await clientAuthHandler.getCredential!()
      if (credential !== null) {
        return credential
      }
    }
    return null
  })
}

// A login or logout inside a provider's own component (Clerk's widget)
// changes who the requests are for: refresh the cached queries.
for (const clientAuthHandler of Object.values(clientAuthHandlers)) {
  clientAuthHandler?.onCredentialChange?.(() => {
    void invalidateAndRemoveQueries()
  })
}
