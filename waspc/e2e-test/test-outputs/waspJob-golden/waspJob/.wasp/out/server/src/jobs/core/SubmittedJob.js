/**
 * This is the result of submitting a Job to some executor.
 * It can be used by callers to track things, or call executor-specific subclass functionality.
 */
export class SubmittedJob {
  #job
  #jobId

  /**
   * @param {Job} job - The Job that submitted work to an executor.
   * @param {string} jobId - A UUID for a submitted job in that executor's ecosystem.
   */
  constructor(job, jobId) {
    this.#job = job
    this.#jobId = jobId
  }

  get jobId() {
    return this.#jobId
  }

  get jobName() {
    return this.#job.jobName
  }

  get executorName() {
    return this.#job.executorName
  }
}
