import PgBoss from 'pg-boss'
import { config, env } from '../../../index.js'

const boss = createPgBoss()

function createPgBoss() {
  let pgBossNewOptions = {
    connectionString: config.databaseUrl,
  }

  // Add an escape hatch for advanced configuration of pg-boss to overwrite our defaults.
  if (env.PG_BOSS_NEW_OPTIONS) {
    try {
      pgBossNewOptions = JSON.parse(env.PG_BOSS_NEW_OPTIONS)
    } catch {
      console.error(
        'Environment variable PG_BOSS_NEW_OPTIONS was not parsable by JSON.parse()!'
      )
    }
  }

  return new PgBoss(pgBossNewOptions)
}

let resolvePgBossStarted: (boss: PgBoss) => void
let rejectPgBossStarted: (boss: PgBoss) => void
// PRIVATE API
// Code that wants to access pg-boss must wait until it has been started.
export const pgBossStarted = new Promise<PgBoss>((resolve, reject) => {
  resolvePgBossStarted = resolve
  rejectPgBossStarted = reject
})

enum PgBossStatus {
  Unstarted = 'Unstarted',
  Starting = 'Starting',
  Started = 'Started',
  Error = 'Error',
}

let pgBossStatus: PgBossStatus = PgBossStatus.Unstarted

// PRIVATE API
/**
 * Prepares the target PostgreSQL database and begins job monitoring.
 * If the required database objects do not exist in the specified database,
 * `boss.start()` will automatically create them.
 * Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
 *
 * After making this call, we can send pg-boss jobs and they will be persisted and acted upon.
 * This should only be called once during a server's lifetime.
 */
export async function startPgBoss(): Promise<void> {
  // Ensure pg-boss can only be started once during a server's lifetime.
  if (pgBossStatus !== PgBossStatus.Unstarted) {
    return
  }
  pgBossStatus = PgBossStatus.Starting
  console.log('Starting pg-boss...')

  boss.on('error', (error) => console.error(error))
  try {
    await boss.start()
  } catch (error) {
    console.error('pg-boss failed to start!')
    console.error(error)
    pgBossStatus = PgBossStatus.Error
    rejectPgBossStarted(boss)
    return
  }

  resolvePgBossStarted(boss)

  console.log('pg-boss started!')
  pgBossStatus = PgBossStatus.Started
}

const lazyPgBossKey = Symbol.for('wasp.jobs.pgBossStarted')

// PRIVATE API
/**
 * Like awaiting `pgBossStarted`, but starts pg-boss lazily when nothing
 * else has: the standalone server process starts it at boot, while the
 * Nitro worker (which serves HTTP requests, including job submissions from
 * operations and `setupFn`) has no boot phase that does. A lazily started
 * instance executes no Wasp jobs: job handlers and schedules register
 * through `registerJob`, which only the standalone process's `allJobs`
 * import runs (pg-boss's own maintenance/cron supervision does run here,
 * which is safe — pg-boss is multi-instance by design).
 *
 * The started promise is cached on `globalThis` because dev reloads
 * re-execute this module, and each fresh module-level `boss` must not be
 * started again.
 *
 * TODO(nitro-phase-4): replaced by the stateful-resource lifecycle once
 * job execution itself moves into the Nitro worker.
 */
export function ensurePgBossStarted(): Promise<PgBoss> {
  const cache = globalThis as Record<symbol, Promise<PgBoss> | undefined>
  let startedPgBoss = cache[lazyPgBossKey]
  if (!startedPgBoss) {
    startPgBoss()
    startedPgBoss = pgBossStarted
    cache[lazyPgBossKey] = startedPgBoss
  }
  return startedPgBoss
}
