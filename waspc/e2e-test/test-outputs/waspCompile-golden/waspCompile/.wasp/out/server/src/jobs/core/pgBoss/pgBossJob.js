import { boss, registerAfterStartCallback } from './pgBoss.js'
import { Job } from '../Job.js'
import { SubmittedJob } from '../SubmittedJob.js'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

/**
 * A PgBoss specific SubmittedJob that adds additional PgBoss functionality.
 */
class PgBossSubmittedJob extends SubmittedJob {
  constructor(job, jobId) {
    super(job, jobId)
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
  #defaultJobOptions
  #startAfter

  /**
   * 
   * @param {string} jobName - The name of the Job. This is what will show up in the pg-boss DB tables.
   * @param {object} defaultJobOptions - Default options passed to `boss.send()`.
   *                                     Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#sendname-data-options
   * @param {int | string | date} startAfter - Defers job execution. See `delay()` below for more.
   */
  constructor(jobName, defaultJobOptions, startAfter = undefined) {
    super(jobName, PG_BOSS_EXECUTOR_NAME)
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
    return new PgBossJob(this.jobName, this.#defaultJobOptions, startAfter)
  }

  /**
   * Submits the job to PgBoss.
   * @param {object} jobArgs - The job arguments supplied by the user for their perform callback.
   * @param {object} jobOptions - PgBoss specific options for `boss.send()`, which can override their defaultJobOptions.
   */
  async submit(jobArgs, jobOptions) {
    const jobId = await boss.send(this.jobName, jobArgs,
      { ...this.#defaultJobOptions, ...(this.#startAfter && { startAfter: this.#startAfter }), ...jobOptions })
    return new PgBossSubmittedJob(this, jobId)
  }
}

/**
 * Creates an instance of PgBossJob and initializes the PgBoss executor by registering this job function.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 * @param {string} jobName - The user-defined job name in their .wasp file.
 * @param {fn} jobFn - The user-defined async job callback function.
 * @param {object} defaultJobOptions - PgBoss specific options for boss.send() applied to every submit() invocation,
 *                                     which can overriden in that call.
 * @param {object} jobSchedule [Optional] - The cron string and arguments/options when invoking the job.
 */
export async function createJob({ jobName, jobFn, defaultJobOptions, jobSchedule } = {}) {
  // Note: createJob runs before PgBoss starts. Therefore, anything that expects PgBoss to be running
  // should be registered as a callback passed to registerAfterStartCallback.
  registerAfterStartCallback(async () => {
    // As a safety precaution against undefined behavior of registering different
    // functions for the same job name, remove all registered functions first.
    await boss.offWork(jobName)

    // This tells pgBoss to run given worker function when job/payload with given job name is submitted.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#work
    await boss.work(jobName, jobFn)

    // If a job schedule is provided, we should schedule the recurring job.
    // If the schedule name already exists, it's updated to the new cron expression.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#scheduling
    if (jobSchedule !== null) {
      await boss.schedule(jobName, jobSchedule.cron, jobSchedule.performFnArg || null, jobSchedule.options || {})
    }
  })

  return new PgBossJob(jobName, defaultJobOptions)
}
