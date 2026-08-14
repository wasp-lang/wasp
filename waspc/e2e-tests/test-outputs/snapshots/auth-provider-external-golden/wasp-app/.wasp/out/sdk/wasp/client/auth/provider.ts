import type { ClientAuthAdapter } from '@wasp.sh/auth-contract/client'

import { createClientAdapter } from '@wasp.sh/auth-clerk/client'

import { apiEventsEmitter } from '../../api/events.js'
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
  undefined,
)

// Rebroadcast provider-side credential changes into Wasp's existing event
// channel, so the current-user query and live websocket connections react to
// token rotation and to logins/logouts that happen inside the provider's own
// UI.
clientAuthAdapter.onCredentialChange?.(() => {
  apiEventsEmitter.emit('sessionId.set')
})
