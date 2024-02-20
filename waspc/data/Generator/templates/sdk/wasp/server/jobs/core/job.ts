/**
 * This is a definition of a job (think draft or invocable computation), not the running instance itself.
 * This can be submitted one or more times to be executed by some job executor via the same instance.
 * Once submitted, you get a SubmittedJob to track it later.
 */
export class Job {
  public readonly jobName: string
  public readonly executorName: string | symbol

  constructor(jobName: string, executorName: string | symbol) {
    this.jobName = jobName
    this.executorName = executorName
  }
}

/**
 * This is the result of submitting a Job to some executor.
 * It can be used by callers to track things, or call executor-specific subclass functionality.
 */
export class SubmittedJob {
  public readonly job: Job
  public readonly jobId: string

  constructor(job: Job, jobId: string) {
    this.job = job
    this.jobId = jobId
  }
}
