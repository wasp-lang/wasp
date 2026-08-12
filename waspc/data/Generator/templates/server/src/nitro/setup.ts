{{={= =}=}}
import { type Server } from 'http'

import { type ServerSetupFn } from 'wasp/server'
import { type ServerSetupFnContext } from 'wasp/server/types'

import app from '../app.js'
{=& setupFn.importStatement =}

const setupFnKey = Symbol.for('wasp.nitro.setupFn')

/**
 * Runs your app's setup function on the Express app Nitro serves the API with,
 * once per app, when the server starts (see `plugins/wasp.ts`).
 *
 * "Once per app" instead of "once", because in development Nitro runs your
 * server's code again on every change, and if that gives us a new Express app,
 * the setup function has to run on it too (a run remembered on `globalThis`
 * would leave the new app without the routes your setup function adds).
 */
export function runSetupFn(): Promise<void> {
  const appProperties = app as unknown as Record<
    symbol,
    Promise<void> | undefined
  >
  appProperties[setupFnKey] ??= callSetupFn().catch((error) => {
    // We don't remember a run that failed, so that the next start tries again
    // (with the fixed setup function, in development).
    appProperties[setupFnKey] = undefined
    throw error
  })
  return appProperties[setupFnKey]
}

async function callSetupFn(): Promise<void> {
  const context: ServerSetupFnContext = {
    app,
    server: makeUnavailableHttpServer(),
  }
  await ({= setupFn.importIdentifier =} as ServerSetupFn)(context)
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
