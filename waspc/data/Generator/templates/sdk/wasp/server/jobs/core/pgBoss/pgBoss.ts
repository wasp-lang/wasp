import PgBoss from 'pg-boss'
import { config, env } from '../../../index.js'

// PRIVATE API
/**
 * The started pg-boss instance.
 *
 * Importing this module starts pg-boss, so only import it dynamically. This
 * way, the server decides when pg-boss starts by importing it during startup,
 * and other code that needs the instance gets it from the same import.
 */
export const boss = await startPgBoss()

/**
 * Prepares the target PostgreSQL database and begins job monitoring.
 * If the required database objects do not exist in the specified database,
 * `boss.start()` will automatically create them.
 * Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
 *
 * After making this call, we can send pg-boss jobs and they will be persisted and acted upon.
 */
async function startPgBoss(): Promise<PgBoss> {
  const boss = createPgBoss()
  boss.on('error', (error) => console.error(error))

  console.log('Starting pg-boss...')
  await boss.start()
  console.log('pg-boss started!')

  return boss
}

function createPgBoss(): PgBoss {
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
