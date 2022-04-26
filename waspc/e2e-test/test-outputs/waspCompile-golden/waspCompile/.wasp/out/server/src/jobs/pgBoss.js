import PgBoss from 'pg-boss'
import config from '../config.js'

export const boss = new PgBoss({ connectionString: config.databaseUrl })

/**
 * Prepares the target PostgreSQL database and begins job monitoring.
 * If the required database objects do not exist in the specified database,
 * start() will automatically create them.
 * After this point, we can send it jobs and they will be persisted and acted upon.
 * Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
 */
export async function startPgBoss() {
  console.log('Starting PgBoss...')
  boss.on('error', error => console.error(error))
  await boss.start()
  console.log('PgBoss started!')
}
