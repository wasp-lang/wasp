{{={= =}=}}
import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'
import type { AuthSchemeName } from '../../auth/scheme.js'
import {
  registerCredentialSource,
  setCredential,
} from '../../api/index.js'
import { invalidateAndRemoveQueries } from '../operations/internal/resources.js'
import { config } from '../config.js'
import { env } from '../env.js'
{=# clientAdapterProviders =}
import { createClientAdapter as createClientAdapter_{= index =} } from '{= clientPackage =}'
{=/ clientAdapterProviders =}

/**
 * The client halves of the app's auth schemes, instantiated from each
 * handler package's client entry with the same runtime-window discipline as
 * the server halves: an adapter sees only what Wasp hands it here. Keyed by
 * scheme name; schemes without a client package simply have no entry.
 */

// PRIVATE API
export const defaultScheme: AuthSchemeName = '{= defaultScheme =}'

// Each adapter's env is narrowed to exactly the vars its manifest declared:
// what an adapter reads is what its manifest shows. The credential sink is
// pre-bound to the scheme name, so an adapter cannot write another scheme's
// logout marker.
function makeClientRuntime(
  scheme: AuthSchemeName,
  declaredClientEnvVarNames: readonly string[],
) {
  return {
    scheme,
    apiUrl: config.apiUrl,
    mountUrl: `${config.apiUrl}/auth/${scheme}`,
    env: Object.fromEntries(
      declaredClientEnvVarNames.map((name) => [
        name,
        (env as Record<string, string | undefined>)[name],
      ]),
    ),
    setCredential: async (credential: string | null): Promise<void> => {
      setCredential(credential, scheme)
      await invalidateAndRemoveQueries()
    },
  }
}

// PRIVATE API
export const clientAuthAdapters: Partial<Record<AuthSchemeName, ClientAuthAdapter>> = {
  {=# clientAdapterProviders =}
  '{= schemeName =}': createClientAdapter_{= index =}(makeClientRuntime('{= schemeName =}', {=& clientEnvVarNamesJs =}), {=# hasOptions =}{=& optionsJson =}{=/ hasOptions =}{=^ hasOptions =}undefined{=/ hasOptions =}),
  {=/ clientAdapterProviders =}
}

// The default scheme's adapter is the request path's fallback credential
// source: when no Wasp-issued credential is stored, its own credential (a
// hosted provider's token, say) rides on every request. No other adapter is
// ever consulted on the wire -- two live credentials never race.
const defaultAdapter = (clientAuthAdapters as Partial<Record<string, ClientAuthAdapter>>)[defaultScheme]
if (defaultAdapter?.getCredential !== undefined) {
  registerCredentialSource(() => defaultAdapter.getCredential!())
}

// A login or logout inside a provider's own component (Clerk's widget)
// changes who the requests are for: refresh the cached queries.
for (const adapter of Object.values(clientAuthAdapters)) {
  adapter?.onCredentialChange?.(() => {
    void invalidateAndRemoveQueries()
  })
}
