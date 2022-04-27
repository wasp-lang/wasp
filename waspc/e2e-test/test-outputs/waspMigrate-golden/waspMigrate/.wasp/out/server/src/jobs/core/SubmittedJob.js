/**
 * This is the result of submitting a job to some executor.
 * It can be used by callers to track things, or call executor-specific subclass functionality.
 */
 export class SubmittedJob {
  /**
   * @param {string} jobName - The name given when constructing the original job.
   * @param {string} jobId - A UUID for a given job in that executor's ecosystem.
   * @param {symbol} executorName - The name of the executor that is running the job.
   */
  constructor(jobName, jobId, executorName) {
    this.jobName = jobName
    this.jobId = jobId
    this.executorName = executorName
  }
}
