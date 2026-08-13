/**
 * Your server doesn't only answer requests: it also runs a job queue, keeps a
 * pool of database connections, and whatever your setup function started. Those
 * must exist exactly once, but in development they live inside Vite's server,
 * which re-runs the code that created them every time you change a file, and
 * never tells the old ones to stop.
 *
 * A stateful resource is Wasp's answer to that. It keeps every resource on
 * `globalThis` (re-running a module doesn't reset that), hands the code of the
 * new run either the same resource or a fresh one (depending on the resource's
 * reload policy), and lets no two starts or stops overlap.
 */

// PRIVATE API
export type ReloadPolicy =
  /**
   * Keep the resource that is already running. For resources nothing captures:
   * a database client, an HTTP client, a cache.
   */
  | 'reuse'
  /**
   * Stop the running resource and start a new one. For resources holding on to
   * your code, which is exactly what changed: a job queue with your job
   * functions registered on it, for example.
   */
  | 'recreate'

// PRIVATE API
export type StatefulResourceOptions<T> = {
  /**
   * Must be atomic: if it fails halfway, it has to clean up after itself before
   * it throws. Nothing else can, since a resource that was never returned is a
   * resource we never got to see.
   */
  create: () => T | Promise<T>
  dispose: (value: T) => void | Promise<void>
  reloadPolicy: ReloadPolicy
  /**
   * A summary of the configuration the resource was created with. When it
   * changes, the resource is recreated even under the `reuse` policy.
   */
  configHash?: string
  /**
   * Refuse to create the resource while the app's pages are being prerendered
   * (which happens during `vite build`, in a server Nitro starts for it).
   */
  disabledDuringPrerender?: boolean
}

// PRIVATE API
export type StatefulResource<T> = {
  readonly key: string
  /** The resource, created if it isn't running yet. */
  get(): Promise<T>
  /**
   * Runs `fn` with the resource, in order with its creation and disposal, so
   * that whatever `fn` does to it can't be undone by a reload happening
   * halfway through.
   */
  use<Result>(fn: (value: T) => Result | Promise<Result>): Promise<Result>
  dispose(): Promise<void>
}

type RegistryEntry = {
  key: string
  configHash: string | undefined
  /** Bumped every time the module defining this resource is executed. */
  generation: number
  /** Always the newest ones, so that a queued creation uses the newest code. */
  create: () => unknown | Promise<unknown>
  dispose: (value: unknown) => void | Promise<void>
  live:
    | {
        value: unknown
        generation: number
        /** The `dispose` of the definition that created `value`. */
        dispose: (value: unknown) => void | Promise<void>
      }
    | undefined
  /** When this resource was last (re)defined, for `waitForDefinitionsToSettle`. */
  lastDefinedAt: number
  /** The chain every start and stop of this resource goes through. Never rejects. */
  tail: Promise<void>
}

/**
 * `Symbol.for` instead of a module-level variable: this module is executed
 * again on every reload (and, in some setups, more than once at the same time),
 * and everything it declares is fresh each time. `globalThis` isn't.
 */
const registryKey = Symbol.for('wasp.statefulResources.v1')

/**
 * How long we wait for a burst of redefinitions to end before acting on it.
 * Saving a few files in a row (or one editor writing several times) otherwise
 * takes the resource through one stop/start cycle per save.
 */
const definitionsSettleMs = 250

// PRIVATE API
export function defineStatefulResource<T>(
  key: string,
  options: StatefulResourceOptions<T>
): StatefulResource<T> {
  const entry = defineEntry(key, options)

  const whenDisabled = options.disabledDuringPrerender && isPrerendering()

  return {
    key,
    get() {
      if (whenDisabled) return rejectAsDisabled(key)
      return enqueue(entry, () => ensureCreated(entry)) as Promise<T>
    },
    use(fn) {
      if (whenDisabled) return rejectAsDisabled(key)
      return enqueue(entry, async () => fn((await ensureCreated(entry)) as T))
    },
    dispose() {
      return enqueue(entry, () => disposeLive(entry))
    },
  }
}

/**
 * The synchronous sibling of `defineStatefulResource`, for resources you can
 * create without waiting (a Prisma client, for example) and whose users expect
 * to have them right away, without awaiting anything.
 *
 * There is no reload policy: a resource you get by asking for it, instead of by
 * awaiting it, is always reused (nothing else can be done synchronously with
 * one that is already running). It is still disposed of when the server stops,
 * and when its configuration changes.
 */
// PRIVATE API
export function defineSyncStatefulResource<T>(
  key: string,
  options: {
    create: () => T
    dispose: (value: T) => void | Promise<void>
    configHash?: string
  }
): T {
  const entry = defineEntry(key, { ...options, reloadPolicy: 'reuse' })

  if (isLiveAndCurrent(entry)) {
    return entry.live!.value as T
  }

  // Only a change of configuration gets us here with a resource still running.
  // We take it out of the entry right away, so that stopping it (which we can't
  // wait for) can't be confused with stopping the one we are about to create.
  const staleLive = entry.live
  entry.live = undefined
  if (staleLive !== undefined) {
    enqueue(entry, async () => staleLive.dispose(staleLive.value)).catch(
      (error) => logDisposeError(key, error)
    )
  }

  const value = options.create()
  entry.live = {
    value,
    generation: entry.generation,
    dispose: options.dispose as (value: unknown) => void | Promise<void>,
  }
  return value
}

/**
 * Stops every resource, newest first (a resource created later may be using one
 * created earlier). Wasp calls this when the server is shutting down.
 */
// PRIVATE API
export async function disposeAllStatefulResources(): Promise<void> {
  for (const entry of [...getRegistry().values()].reverse()) {
    try {
      await enqueue(entry, () => disposeLive(entry))
    } catch (error) {
      logDisposeError(entry.key, error)
    }
  }
}

function defineEntry<T>(
  key: string,
  options: StatefulResourceOptions<T>
): RegistryEntry {
  const registry = getRegistry()
  const existingEntry = registry.get(key)

  if (existingEntry === undefined) {
    const entry: RegistryEntry = {
      key,
      configHash: options.configHash,
      generation: 1,
      create: options.create,
      dispose: options.dispose as (value: unknown) => void | Promise<void>,
      live: undefined,
      lastDefinedAt: Date.now(),
      tail: Promise.resolve(),
    }
    registry.set(key, entry)
    return entry
  }

  // We are being defined again, which means our module was executed again: a
  // reload, or a second copy of the SDK. Either way, the code that just ran is
  // the newest one there is, so we take its `create` and `dispose`.
  const isConfigChanged = existingEntry.configHash !== options.configHash
  existingEntry.configHash = options.configHash
  existingEntry.create = options.create
  existingEntry.dispose = options.dispose as (
    value: unknown
  ) => void | Promise<void>
  existingEntry.generation += 1
  existingEntry.lastDefinedAt = Date.now()

  if (options.reloadPolicy === 'recreate' || isConfigChanged) {
    // Queued before anything this definition asks for, so that the resource of
    // the previous definition is gone before its replacement is created.
    enqueue(existingEntry, () => disposeStale(existingEntry)).catch((error) =>
      logDisposeError(key, error)
    )
  } else {
    // `reuse`: the running resource becomes this definition's resource, or
    // every reload would recreate it after all.
    if (existingEntry.live !== undefined) {
      existingEntry.live.generation = existingEntry.generation
    }
  }

  return existingEntry
}

/**
 * Adds `operation` to the resource's chain, so that it runs once everything
 * asked for before it is done, even if some of that failed.
 */
function enqueue<T>(
  entry: RegistryEntry,
  operation: () => Promise<T>
): Promise<T> {
  const result = entry.tail.then(operation, operation)
  entry.tail = result.then(ignore, ignore)
  return result
}

async function ensureCreated(entry: RegistryEntry): Promise<unknown> {
  if (isLiveAndCurrent(entry)) {
    return entry.live!.value
  }

  if (entry.live !== undefined) {
    await waitForDefinitionsToSettle(entry)
    // The wait is long enough for somebody else to have done the work.
    if (isLiveAndCurrent(entry)) {
      return entry.live!.value
    }
    await disposeLive(entry)
  }

  // Read before creating and remembered with the value: a definition arriving
  // while we create must not make us look like we created what it defined.
  const { create, dispose, generation } = entry
  const value = await create()
  entry.live = { value, generation, dispose }
  return value
}

/**
 * Disposes of the resource of a definition that has been superseded. Also runs
 * (as a no-op) when somebody got there first, because every definition queues
 * one of these.
 */
async function disposeStale(entry: RegistryEntry): Promise<void> {
  await waitForDefinitionsToSettle(entry)
  if (entry.live !== undefined && !isLiveAndCurrent(entry)) {
    await disposeLive(entry)
  }
}

async function disposeLive(entry: RegistryEntry): Promise<void> {
  const live = entry.live
  if (live === undefined) return
  // Forgotten before it is stopped: whatever happens while stopping it, nobody
  // gets to use it again.
  entry.live = undefined
  await live.dispose(live.value)
}

function isLiveAndCurrent(entry: RegistryEntry): boolean {
  return entry.live !== undefined && entry.live.generation === entry.generation
}

/**
 * Waits until this resource has been left alone for a moment. Saving five files
 * in a row redefines it five times, and without this each one of those would
 * take it through a full stop/start cycle.
 */
async function waitForDefinitionsToSettle(entry: RegistryEntry): Promise<void> {
  for (
    let quietFor = Date.now() - entry.lastDefinedAt;
    quietFor < definitionsSettleMs;
    quietFor = Date.now() - entry.lastDefinedAt
  ) {
    await sleep(definitionsSettleMs - quietFor)
  }
}

function getRegistry(): Map<string, RegistryEntry> {
  const globalObject = globalThis as typeof globalThis & {
    [registryKey]?: Map<string, RegistryEntry>
  }
  return (globalObject[registryKey] ??= new Map())
}

function isPrerendering(): boolean {
  // Nitro defines this only in the code it builds, which is why the TypeScript
  // setups compiling this file don't know about it.
  return (import.meta as ImportMeta & { prerender?: boolean }).prerender === true
}

function rejectAsDisabled<T>(key: string): Promise<T> {
  return Promise.reject(
    new Error(
      `The "${key}" resource is not available while the app's pages are being prerendered.`
    )
  )
}

function logDisposeError(key: string, error: unknown): void {
  console.error(`Failed to dispose of the "${key}" resource.`, error)
}

function sleep(milliseconds: number): Promise<void> {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds)
  })
}

function ignore(): void {}
