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
