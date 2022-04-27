/**
 * This is the result of submitting a job definition to some executor.
 * It can be used by callers to track things, or call subclass functionality.
 */
export class SubmittedJob {
  /**
   * @param {string} jobName - The name given when constructing the original Job.
   * @param {string} jobId - A UUID for a given Job in that executor's ecosystem.
   * @param {string} executorName - The name of the executor that is running the job.
   */
  constructor(jobName, jobId, executorName) {
    this.jobName = jobName
    this.jobId = jobId
    this.executorName = executorName
  }
}
