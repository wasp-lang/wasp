import { definePlugin } from 'nitro'
import { type NitroAppPlugin } from 'nitro/types'

import { disposeAllStatefulResources } from 'wasp/server/lifecycle'

/** The Nitro app a plugin gets, which always has the hooks. */
type NitroApp = Parameters<NitroAppPlugin>[0]

const startupKey = Symbol.for('wasp.nitro.startup')
const shutdownHandlersKey = Symbol.for('wasp.nitro.shutdownHandlers')
const closeHookKey = Symbol.for('wasp.nitro.closeHook')

/**
 * Everything of your app's server that isn't answering a request: its job
 * queue, your server setup function, and stopping all of it when the server
 * stops.
 *
 * Nitro runs this when the server starts, and again after every change to your
 * server code in development (where the server code is re-run, in the same
 * process, without anything being stopped first). That is why the long-lived
 * parts of it are stateful resources (`wasp/server/lifecycle`): they know how
 * to survive, or replace themselves on, a reload.
 */
export default definePlugin((nitroApp) => {
  installShutdownHandlers()
  installCloseHook(nitroApp)

  // Nitro's plugins are synchronous, so this is a promise nobody waits for: the
  // server starts answering requests while your app's server side is starting.
  // Requests that need it to be up wait for it (see `serverEntry.ts`).
  void startWaspServer()
})

// PRIVATE API
/**
 * Resolves once the app's server side is up: its jobs are registered and your
 * setup function has run. Starts it if nothing else has (which the plugin
 * above normally does, when the server starts).
 *
 * It doesn't reject: a server side that failed to start says so in the logs,
 * and requests carry on without it (in development, saving a file starts it
 * again).
 */
export function waspServerStarted(): Promise<void> {
  return getGlobalProperties()[startupKey] ?? startWaspServer()
}

function startWaspServer(): Promise<void> {
  const startup = startWaspServerOnce().catch((error) => {
    console.error('Your app failed to start.', error)
  })
  getGlobalProperties()[startupKey] = startup
  return startup
}

async function startWaspServerOnce(): Promise<void> {
  // Nitro renders the app's prerendered pages at build time, by starting the
  // app's server and asking it for them. A database and a job queue are a
  // deployment's business, and aren't around at build time.
  if (isPrerendering()) {
    return
  }

  try {
    // Loads the app's jobs (which register themselves) before job execution
    // starts.
    const { startJobExecution } = await import('wasp/server/jobs/core/pgBoss')
    await import('../../jobs/core/allJobs.js')
    await startJobExecution()
  } catch (error) {
    // An app whose job queue is down (its database isn't up yet, for example)
    // still serves requests. Submitting a job tries to start it again.
    console.error("Your app's jobs failed to start.", error)
  }

  const { runSetupFn } = await import('../setup.js')
  await runSetupFn()
}

/**
 * Nitro's node server never turns a signal into its `close` hook, and neither
 * does its development server, so we stop the app's resources ourselves.
 *
 * Registered once per process: this plugin runs again on every reload in
 * development, and a listener per reload would both pile up and (once the
 * process has more than ten of them) have Node warn about a leak.
 */
function installShutdownHandlers(): void {
  const globalProperties = getGlobalProperties()
  if (globalProperties[shutdownHandlersKey]) {
    return
  }
  globalProperties[shutdownHandlersKey] = true

  let isShuttingDown = false
  const shutDown = (signal: NodeJS.Signals) => {
    if (isShuttingDown) {
      return
    }
    isShuttingDown = true
    console.log(`Received ${signal}, stopping the app...`)
    void disposeAllStatefulResources().then(
      () => console.log('The app stopped.'),
      (error) => console.error('The app failed to stop cleanly.', error)
    )
  }

  process.on('SIGTERM', shutDown)
  process.on('SIGINT', shutDown)
  // No `process.exit()` in there: once its resources are gone, nothing holds
  // the process open anymore, and it ends on its own, after everything else
  // (Nitro's own shutdown included) had its chance to finish.
}

/**
 * The way Nitro says it is shutting down. It doesn't say it in any of the ways
 * an app actually stops today (which is what the signal handlers above are
 * for), so this is only a second path, for when it starts to.
 */
function installCloseHook(nitroApp: NitroApp): void {
  const nitroAppProperties = nitroApp as NitroApp & {
    [closeHookKey]?: true
  }
  if (nitroAppProperties[closeHookKey]) {
    return
  }
  nitroAppProperties[closeHookKey] = true
  nitroApp.hooks.hook('close', () => disposeAllStatefulResources())
}

/**
 * The properties Wasp keeps on `globalThis`, which is the only thing a reload
 * doesn't replace.
 */
function getGlobalProperties(): typeof globalThis & {
  [startupKey]?: Promise<void>
  [shutdownHandlersKey]?: true
} {
  return globalThis
}

function isPrerendering(): boolean {
  // Nitro's, and only defined in the code it builds, which is why the generated
  // server's TypeScript setup doesn't know about it.
  return (import.meta as ImportMeta & { prerender?: boolean }).prerender === true
}
