import { boss } from './pgBoss.js'
import { Job } from '../Job.js'
import { SubmittedJob } from '../SubmittedJob.js'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

/**
 * A PgBoss specific SubmittedJob that adds additional PgBoss functionality.
 */
class PgBossSubmittedJob extends SubmittedJob {
  constructor(jobName, jobId) {
    super(jobName, jobId, PG_BOSS_EXECUTOR_NAME)
    this.pgBoss = {
      async cancel() { return boss.cancel(jobId) },
      async resume() { return boss.resume(jobId) },
      async details() { return boss.getJobById(jobId) }
    }
  }
}

/**
 * This is a class repesenting a job that can be submitted to PgBoss.
 * It is not yet submitted until the caller invokes `submit()` on an instance.
 * The caller can make as many calls to `submit()` as they wish.
 */
class PgBossJob extends Job {
  #defaultJobOptions = {}
  #startAfter = 0

  constructor(jobName, defaultJobOptions, startAfter = 0) {
    super(jobName)
    this.#defaultJobOptions = defaultJobOptions
    this.#startAfter = startAfter
  }

  /**
   * @param {int | string | date} startAfter - Defers job execution by either:
   * - int: Seconds to delay starting the job [Default: 0]
   * - string: Start after a UTC Date time string in 8601 format
   * - Date: Start after a Date object
   */
  delay(startAfter) {
    return new PgBossJob(this.jobName(), this.#defaultJobOptions, startAfter)
  }

  /**
   * Submits the job to PgBoss.
   * @param {object} jobArgs - The job arguments supplied by the user for their perform callback.
   * @param {string} jobOptions - PgBoss specific options for `boss.send()`, which can override their defaultJobOptions.
   */
  async submit(jobArgs, jobOptions) {
    const jobId = await boss.send(this.jobName(), jobArgs, { ...this.#defaultJobOptions, startAfter: this.#startAfter, ...jobOptions })
    return new PgBossSubmittedJob(this.jobName(), jobId)
  }
}

/**
 * Creates an instance of PgBossJob and initializes the PgBoss executor by registering this job function.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 * @param {object} jobName - The user-defined job name in their .wasp file.
 * @param {string} jobFn - The user-defined async job callback function.
 * @param {string} defaultJobOptions - PgBoss specific options for boss.send() applied to every submit() invocation,
 *                                     which can overriden in that call.
 */
export async function createJob({ jobName, jobFn, defaultJobOptions } = {}) {
  // As a safety precaution against undefined behavior of registering different
  // functions for the same job name, remove all registered functions first.
  await boss.offWork(jobName)

  // Adds a new polling worker for a queue and executes the provided callback
  // function when jobs are found. Multiple workers can be added if needed.
  // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#workP
  await boss.work(jobName, jobFn)

  return new PgBossJob(jobName, defaultJobOptions)
}
