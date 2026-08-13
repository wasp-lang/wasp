import { type Server } from 'http'

import { type ServerSetupFn } from 'wasp/server'
import { type ServerSetupFnContext } from 'wasp/server/types'

import app from '../app.js'
import { serverSetup } from '../../../../../src/serverSetup'

const setupFnPromiseKey = Symbol.for('wasp.nitro.setupFnPromise')

/**
 * Runs your app's `setupFn` on the Express app Nitro serves the API with.
 *
 * We run it lazily, on the first API request, and remember the promise on the
 * app itself so that it runs exactly once per app instance (Nitro re-executes
 * the server code on every change in development).
 *
 * NOTE: While Wasp is moving its server onto Nitro, Wasp still runs a second
 * server process next to Nitro (for jobs and websockets), and that process runs
 * your `setupFn` too. So in development it runs twice, once per process. Keep
 * that in mind if your `setupFn` does something beyond configuring the Express
 * app (sending a request, writing to the database, starting a timer, ...).
 */
export function ensureSetupFnHasRun(): Promise<void> {
  const appProperties = app as unknown as Record<symbol, Promise<void> | undefined>
  appProperties[setupFnPromiseKey] ??= runSetupFn().catch((error) => {
    // We don't remember a run that failed, so that the next request tries again
    // (with the fixed setup function, in development).
    appProperties[setupFnPromiseKey] = undefined
    throw error
  })
  return appProperties[setupFnPromiseKey]
}

async function runSetupFn(): Promise<void> {
  const context: ServerSetupFnContext = {
    app,
    server: makeUnavailableHttpServer(),
  }
  await (serverSetup as ServerSetupFn)(context)
}

/**
 * Nitro owns the HTTP server, and doesn't hand it out. Setup functions that
 * only configure the Express app (the vast majority) don't care, so instead of
 * refusing to run them all we let them run and fail loudly, with an explanation,
 * if they reach for the server.
 */
function makeUnavailableHttpServer(): Server {
  return new Proxy({} as Server, {
    get(_target, property) {
      // Let the runtime inspect the object (`console.log`, `util.inspect`, ...)
      // without blowing up.
      if (typeof property === 'symbol') {
        return undefined
      }
      throw new Error(
        `Your server setup function used \`server.${property}\`, but the HTTP server is not available anymore: ` +
          'Wasp now serves your app with Nitro, which owns the server. ' +
          'Please remove the usage of `server` from your setup function.'
      )
    },
  })
}
