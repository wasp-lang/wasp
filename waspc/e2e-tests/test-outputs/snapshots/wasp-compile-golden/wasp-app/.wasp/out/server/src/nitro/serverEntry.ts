import { defineHandler } from 'nitro/h3'

import { bridgedPathPrefixes } from './apiManifest.js'

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
    return undefined
  }

  if (!isBridgedPath(event.url.pathname)) {
    return undefined
  }

  // Imported lazily, so that the app's pages can be rendered without the API
  // (and everything it needs) being loaded. See the prerendering note above.
  const { bridge } = await import('./expressBridge.js')

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
