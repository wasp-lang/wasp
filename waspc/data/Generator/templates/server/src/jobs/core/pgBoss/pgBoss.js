import PgBoss from 'pg-boss'
import config from '../../../config.js'

const boss = createPgBoss()

function createPgBoss() {
  let pgBossNewOptions = {
    connectionString: config.databaseUrl,
  }

  // Add an escape hatch for advanced configuration of pg-boss to overwrite our defaults.
  if (process.env.PG_BOSS_NEW_OPTIONS) {
    try {
      pgBossNewOptions = JSON.parse(process.env.PG_BOSS_NEW_OPTIONS)
    }
    catch {
      console.error("Environment variable PG_BOSS_NEW_OPTIONS was not parsable by JSON.parse()!")
    }
  }

  return new PgBoss(pgBossNewOptions)
}

let resolvePgBossStarted, rejectPgBossStarted
// Code that wants to access pg-boss must wait until it has been started.
export const pgBossStarted = new Promise((resolve, reject) => {
  resolvePgBossStarted = resolve
  rejectPgBossStarted = reject
})

// Ensure pg-boss can only be started once during a server's lifetime.
const PgBossStatus = {
  Unstarted: 'Unstarted',
  Starting: 'Starting',
  Started: 'Started',
  Error: 'Error'
}
let pgBossStatus = PgBossStatus.Unstarted

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
  if (pgBossStatus !== PgBossStatus.Unstarted) { return }
  pgBossStatus = PgBossStatus.Starting
  console.log('Starting pg-boss...')

  boss.on('error', handlePgBossError)
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

let errConnectionRetries = 0
let errConnectionWindowStartTime = Date.now()

// In the unlikely event that the database becomes unreachable, we do not want the app
// itself to die. This error handler will shut pg-boss down after several connection
// refused errors within a short time period to prevent an untrapped `pg` error from killing
// node (and thus, the entire app). Ref: https://github.com/timgit/pg-boss/issues/365
//
// NOTE: It would be nice if we had a global way to indicate the Wasp app was in a degraded state.
function handlePgBossError(error) {
  console.error('pg-boss error:', error)

  if (error.code === 'ECONNREFUSED') {
    // If it has been more than 60 seconds since the last erorr, reset tracking.
    // If there was a real problem, we would see dozens in a few seconds.
    // This allows for periodic connection blips to not accumulate over time.
    if (elapsedSeconds(errConnectionWindowStartTime) > 60) {
      errConnectionRetries = 0
      errConnectionWindowStartTime = Date.now()
    }

    errConnectionRetries++
  }

  if (errConnectionRetries > 5) {
    console.error(`Connection lost to postgres after ${errConnectionRetries} retries.  Stopping pg-boss...`)

    boss.stop().catch(error => console.error('Error stopping pg-boss:', error))

    const oneMinute = 1 * 60 * 1000
    setInterval(() => console.log(`WARNING: pg-boss was stopped due to postgres connection errors and is no longer running!`), oneMinute)
  }
}

function elapsedSeconds(start) {
  const millis = Date.now() - start;
  return Math.floor(millis / 1000);
}
