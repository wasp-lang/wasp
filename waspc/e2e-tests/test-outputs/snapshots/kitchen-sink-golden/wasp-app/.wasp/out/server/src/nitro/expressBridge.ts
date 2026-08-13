import { fromNodeHandler } from 'nitro/h3'

import app from '../app.js'

/**
 * Wasp's API is an Express app, and Nitro speaks h3, so we hand the app to h3's
 * Node bridge. The bridge gives Express the request's real Node
 * `IncomingMessage`/`ServerResponse` (in development too, where Nitro runs the
 * server code in a worker behind a proxy), so everything Express does with them
 * (streaming, multipart bodies, multiple `Set-Cookie` headers, ...) keeps
 * working as it does today.
 *
 * Express takes three arguments, which is how h3 recognizes it as middleware
 * and gives it a `next()`. That means a request Express doesn't have a route
 * for doesn't become a 404: the bridge resolves with `undefined` instead, and
 * Nitro moves on to rendering a page.
 *
 * We wrap the app once, here at module scope, because the bridge itself holds
 * no per-request state.
 */
export const bridge = fromNodeHandler(app as Parameters<typeof fromNodeHandler>[0])
