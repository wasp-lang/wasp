export class SubmittedJob {
  /**
   * @param {string} jobName - The user-defined job name in their .wasp file.
   * @param {string} jobId - A UUID for a given job in that executor's ecosystem.
   * @param {string} executor - The name of the executor that is running the job.
   */
  constructor(jobName, jobId, executor) {
    this.jobName = jobName
    this.jobId = jobId
    this.executor = executor
  }
}
