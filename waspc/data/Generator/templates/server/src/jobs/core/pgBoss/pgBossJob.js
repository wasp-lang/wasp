import { boss } from './pgBoss.js'
import { Job } from '../Job.js'
import { SubmittedJob } from '../SubmittedJob.js'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

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

  constructor(jobName, defaultJobOptions, delaySeconds = 0) {
    super(jobName, delaySeconds)
    this.#defaultJobOptions = defaultJobOptions
  }

  defaultJobOptions() {
    return this.#defaultJobOptions
  }

  delay(delaySeconds) {
    return new PgBossJob(this.jobName(), this.defaultJobOptions(), delaySeconds)
  }

  /**
   * Submits the job to PgBoss.
   * @param {object} jobArgs - The job arguments supplied by the user for their callback.
   * @param {string} jobOptions - PgBoss specific options for boss.send(), which can override their defaultJobOptions
   *                              specified by their Wasp file.
   */
  async submit(jobArgs, jobOptions) {
    const jobId = await boss.send(this.jobName(), jobArgs, { ...this.defaultJobOptions(), startAfter: this.delaySeconds(), ...jobOptions })
    return new PgBossSubmittedJob(this.jobName(), jobId)
  }
}

/**
 * Creates an instance of PgBossJob and initializes the PgBoss executor by registering this job function.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the first call.
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
