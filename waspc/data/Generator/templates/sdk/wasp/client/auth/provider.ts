{{={= =}=}}
import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'

import { createClientAdapter } from '{= clientPackage =}'

import { config } from '../config.js'
import { env } from '../env.js'

// PRIVATE API
/**
 * The client half of the app's auth provider, instantiated from the adapter
 * package's client entry with the same runtime-window discipline as the
 * server half: the adapter sees only what Wasp hands it here.
 */
export const clientAuthAdapter: ClientAuthAdapter = createClientAdapter(
  {
    apiUrl: config.apiUrl,
    env,
  },
  {=# hasOptions =}{=& optionsJson =}{=/ hasOptions =}{=^ hasOptions =}undefined{=/ hasOptions =},
)

// Provider-side credential changes (token rotation, logins and logouts inside
// the provider's own UI) are observed by the API layer, which exchanges a new
// credential for a Wasp session or ends the session on provider logout. The
// subscription lives in `wasp/api` to keep this module import-cycle-free.
