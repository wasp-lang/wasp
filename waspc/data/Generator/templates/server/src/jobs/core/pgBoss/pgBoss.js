import PgBoss from 'pg-boss'
import config from '../../../config.js'

const boss = createPgBoss()

function createPgBoss() {
  let pgBossNewOptions = { connectionString: config.databaseUrl }
  try {
    // Add an escape hatch for advanced configuration of pg-boss.
    if (process.env.PG_BOSS_NEW_OPTIONS) {
      pgBossNewOptions = JSON.parse(process.env.PG_BOSS_NEW_OPTIONS)
    }
  } catch (error) {
    console.error("Environment variable PG_BOSS_NEW_OPTIONS was not parsable by JSON.parse().")
  }

  return new PgBoss(pgBossNewOptions)
}

let resolvePgBossStarted
// Code that wants to access pg-boss must wait until it has been started.
export const pgBossStarted = new Promise((resolve, _reject) => {
  resolvePgBossStarted = resolve
})

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
    console.log('Starting pg-boss...')

    boss.on('error', error => console.error(error))
    await boss.start()

    resolvePgBossStarted(boss)

    console.log('pg-boss started!')
    hasPgBossBeenStarted = true
  }
}
