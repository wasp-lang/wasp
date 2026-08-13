{{={= =}=}}
import { defineWebSocketHandler } from 'nitro/h3'

import { createWebSocketHooks } from 'wasp/server/webSocket'

{=& userWebSocketFn.importStatement =}

/**
 * The route Nitro accepts your app's websocket connections on. Wasp registers
 * it in the app's Vite config, next to the flag turning Nitro's websocket
 * support on.
 *
 * Everything around your websocket definition (authenticating the connection,
 * keeping track of it so that the rest of your server can reach it, turning the
 * messages it carries into your events) lives in `wasp/server/webSocket`.
 */
export default defineWebSocketHandler(
  createWebSocketHooks({= userWebSocketFn.importIdentifier =})
)
