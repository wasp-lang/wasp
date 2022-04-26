import { boss } from './pgBoss.js'
import { SubmittedJob } from './SubmittedJob.js'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

class PgBossSubmittedJob extends SubmittedJob {
  /**
   * @param {string} jobName - The user-defined job name in their .wasp file.
   * @param {string} jobId - A UUID for a given job in that executor's ecosystem.
   * @param {string} executor - The name of the executor that is running the job.
   * @param {object} pgBoss - PgBoss specific helpers, e.g., cancel()/resume()/details()
   *                          NOTE: This param will see some flux, so not making a proper class right now.
   */
  constructor(jobName, jobId, executor, pgBoss) {
    super(jobName, jobId, executor)
    this.pgBoss = pgBoss
  }
}

class PgBossJob {
  constructor(values) {
    this.jobFn = () => { }
    this.defaultJobOptions = {}
    this.startAfter = 0
    this.jobName = 'unknown'
    Object.assign(this, values)
  }

  delay(startAfter) {
    return new PgBossJob({ ...this, startAfter })
  }

  /**
   * Submits the job to PgBoss.
   * @param {object} jobArgs - The job arguments supplied by the user for their callback.
   * @param {string} jobOptions - PgBoss specific options for boss.send(), which can override their defaultJobOptions
   *                              specified by their Wasp file.
   */
  async submit(jobArgs, jobOptions) {
    const jobId = await boss.send(this.jobName, jobArgs, { ...this.defaultJobOptions, startAfter: this.startAfter, ...jobOptions })
    return new PgBossSubmittedJob(this.jobName, jobId, PG_BOSS_EXECUTOR_NAME, {
      async cancel() { return boss.cancel(jobId) },
      async resume() { return boss.resume(jobId) },
      async details() { return boss.getJobById(jobId) }
    })
  }
}

/**
 * Initialized the PgBoss executor by registering this job function.
 * @param {object} jobName - The user-defined job name in their .wasp file.
 * @param {string} jobFn - The user-defined async job callback function.
 */
export async function executorSetup({ jobName, jobFn } = {}) {
  // Adds a new polling worker for a queue and executes the provided callback
  // function when jobs are found. Multiple workers can be added if needed.
  // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#workP
  await boss.work(jobName, jobFn)
}

/**
 * Creates an instance of PgBossJob.
 * @param {object} jobName - The user-defined job name in their .wasp file.
 * @param {string} jobFn - The user-defined async job callback function.
 * @param {string} defaultJobOptions - PgBoss specific options for boss.send() applied to every submit() invocation,
 *                                     which can overriden in that call.
 */
export function createJob({ jobName, jobFn, defaultJobOptions } = {}) {
  return new PgBossJob({ jobName, jobFn, defaultJobOptions })
}
