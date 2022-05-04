import PgBoss from 'pg-boss'
import config from '../../../config.js'

// Add an escape hatch for advanced configuration of pg-boss.
const pgBossNewOptions = process.env.PG_BOSS_NEW_OPTIONS || {}
export const boss = new PgBoss({ connectionString: config.databaseUrl, ...pgBossNewOptions })

// Allows setup code that runs before pg-boss starts to register their pg-boss functions.
let afterStartCallbacks = []
export function registerAfterStartCallback(callback) {
  afterStartCallbacks.push(callback)
}

// Ensure pg-boss can only be started once during a server's lifetime.
let hasPgBossBeenStarted = false

/**
 * Prepares the target PostgreSQL database and begins job monitoring.
 * If the required database objects do not exist in the specified database,
 * `boss.start()` will automatically create them.
 * Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
 * 
 * After making this call, we can send pg-boss jobs and they will be persisted and acted upon.
 * This should only be called once during a server's lifetime.
 */
export async function startPgBoss() {
  if (!hasPgBossBeenStarted) {
    console.log('Starting PgBoss...')

    boss.on('error', error => console.error(error))
    await boss.start()

    afterStartCallbacks.forEach(fn => fn())

    console.log('PgBoss started!')
    hasPgBossBeenStarted = true
  }
}
