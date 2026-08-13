import { defineHandler, defineWebSocketHandler } from 'nitro/h3'

import { bridgedPathPrefixes } from './apiManifest.js'

/**
 * Nitro accepts a websocket upgrade on *any* path once its websocket support is
 * on, even one nothing is listening on, and leaves it open with no handler
 * attached. Refusing it is only possible from inside a websocket handler, so
 * this one stands in for all the paths that aren't the app's websocket (that
 * one has a route of its own, which Nitro matches before it gets here).
 */
const rejectWebSocketUpgrade = defineWebSocketHandler({
  upgrade() {
    throw new Response('Not Found', { status: 404 })
  },
})

/**
 * Nitro runs this for every request it hasn't already answered with a static
 * file or one of its own routes, and before it renders a page. Returning
 * `undefined` means "not mine", and lets Nitro carry on.
 */
export default defineHandler(async (event) => {
  // Nitro renders the app's prerendered pages at build time, by starting the
  // app's server and asking it for them. The API is not part of that, and
  // starting it would need the server's environment variables, which are a
  // deployment's business and aren't around at build time.
  if (isPrerendering()) {
    return undefined
  }

  // WebSocket upgrades reach us without the Node request/response pair the
  // Express bridge needs (in development). They are never Express's anyway.
  if (event.req.headers.get('upgrade')?.toLowerCase() === 'websocket') {
    return rejectWebSocketUpgrade(event)
  }

  if (!isBridgedPath(event.url.pathname)) {
    return undefined
  }

  // Imported lazily, so that the app's pages can be rendered without the API
  // (and everything it needs) being loaded. See the prerendering note above.
  const { bridge } = await import('./expressBridge.js')

  // Your setup function can add routes to the Express app, so the app only
  // answers requests once it has run (it runs when the server starts).
  const { waspServerStarted } = await import('./plugins/wasp.js')
  await waspServerStarted()

  const nodeRes = event.runtime?.node?.res
  const headerNamesBeforeExpress = new Set(nodeRes?.getHeaderNames())

  const response = await bridge(event)

  if (response === undefined && nodeRes !== undefined) {
    // Express didn't answer the request, so Nitro is about to render a page for
    // it. The middleware Express ran on the way (helmet, cors, ...) has already
    // set its headers on the response though, and an API's headers (a strict
    // content security policy, for example) have no business on an HTML page.
    for (const headerName of nodeRes.getHeaderNames()) {
      if (!headerNamesBeforeExpress.has(headerName)) {
        nodeRes.removeHeader(headerName)
      }
    }
  }

  return response
})

function isBridgedPath(pathname: string): boolean {
  return bridgedPathPrefixes.some((prefix) =>
    pathname === prefix || pathname.startsWith(prefix.endsWith('/') ? prefix : `${prefix}/`)
  )
}

function isPrerendering(): boolean {
  // Nitro's, and only defined in the code it builds, which is why the generated
  // server's TypeScript setup doesn't know about it.
  return (import.meta as ImportMeta & { prerender?: boolean }).prerender === true
}
