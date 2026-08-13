import PgBoss from 'pg-boss'
import { config, env } from '../../../index.js'
import { defineStatefulResource } from '../../../lifecycle/index.js'

// PRIVATE API
/**
 * The app's job queue: a pg-boss instance with every job of the app registered
 * on it (and its schedules kept up to date).
 *
 * It is a stateful resource with a `recreate` policy because the functions
 * pg-boss runs are your job functions: an instance carrying the previous
 * version of your code has to go when your code changes in development.
 */
export const pgBossJobQueue = defineStatefulResource<PgBoss>('pgBoss', {
  reloadPolicy: 'recreate',
  // The escape hatch below is the only thing that can make two pg-boss
  // instances of ours differ.
  configHash: env.PG_BOSS_NEW_OPTIONS ?? '',
  disabledDuringPrerender: true,
  create: startPgBoss,
  dispose: stopPgBoss,
})

/**
 * How a job of the app is registered on a pg-boss instance: telling it which
 * function to run for the job, and when to run it on its own.
 */
type JobRegistration = {
  jobName: string
  registerOn: (boss: PgBoss) => Promise<void>
}

/**
 * Every job of the app, by name. Kept out of the module scope (which a reload
 * replaces) so that a pg-boss instance created after a reload still finds the
 * jobs registered before it.
 */
const jobRegistrationsKey = Symbol.for('wasp.jobs.registrations')

// PRIVATE API
/**
 * Remembers a job so that it is registered on the app's pg-boss instance,
 * whichever one is running by the time job execution starts.
 *
 * We expect this to be called once per job name. Calling it again with a
 * different function (which is what a reload does) replaces the previous one.
 */
export function addJobRegistration(registration: JobRegistration): void {
  getJobRegistrations().set(registration.jobName, registration)
}

// PRIVATE API
/**
 * Starts the app's job queue and hands it every job the app has registered, so
 * that submitted jobs are executed and schedules are running.
 *
 * Called once per run of the server's code: when the server starts, and again
 * after every reload in development, where it registers the app's jobs (with
 * their newest code) on the pg-boss instance of the new generation.
 */
export async function startJobExecution(): Promise<void> {
  await pgBossJobQueue.use(registerJobsOn)
}

async function startPgBoss(): Promise<PgBoss> {
  console.log('Starting pg-boss...')

  const boss = createPgBoss()
  boss.on('error', (error) => console.error(error))

  try {
    // Prepares the target PostgreSQL database (creating the objects pg-boss
    // needs, if they aren't there yet) and begins job monitoring.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
    await boss.start()
    await registerJobsOn(boss)
  } catch (error) {
    // A pg-boss that failed halfway through starting still holds on to database
    // connections, and nobody but us knows about it: we never returned it.
    await stopPgBoss(boss).catch(() => {})
    throw error
  }

  console.log('pg-boss started!')
  return boss
}

async function stopPgBoss(boss: PgBoss): Promise<void> {
  console.log('Stopping pg-boss...')
  const stopped = new Promise<void>((resolve) => boss.once('stopped', resolve))
  await boss.stop({ destroy: true })
  await stopped
  console.log('pg-boss stopped!')
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

async function registerJobsOn(boss: PgBoss): Promise<void> {
  for (const registration of getJobRegistrations().values()) {
    await registration.registerOn(boss)
  }
}

function getJobRegistrations(): Map<string, JobRegistration> {
  const globalObject = globalThis as typeof globalThis & {
    [jobRegistrationsKey]?: Map<string, JobRegistration>
  }
  return (globalObject[jobRegistrationsKey] ??= new Map())
}
