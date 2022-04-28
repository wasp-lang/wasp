import { sleep } from '../../utils.js'
import { Job } from './Job.js'
import { SubmittedJob } from './SubmittedJob.js'

export const PASSTHROUGH_EXECUTOR_NAME = Symbol('Passthrough')

/**
 * A simple job wrapper, mainly to be used for testing.
 */
class PassthroughJob extends Job {
  #jobFn
  #delaySeconds = 0

  constructor(jobName, jobFn, delaySeconds = 0) {
    super(jobName)
    this.#jobFn = jobFn
    this.#delaySeconds = delaySeconds
  }

  /**
   * @param {int} delaySeconds - Used to delay the processing of the job by some number of seconds.
   */
  delay(delaySeconds) {
    return new PassthroughJob(this.jobName(), this.#jobFn, delaySeconds)
  }

  async submit(jobArgs) {
    sleep(this.#delaySeconds * 1000).then(() => this.#jobFn(jobArgs))
    // NOTE: Dumb random ID generator, mainly so we don't have to add `uuid`
    // as a dependency in the server generator for something nobody will likely use.
    let jobId = (Math.random() + 1).toString(36).substring(7)
    return new SubmittedJob(this.jobName(), jobId, PASSTHROUGH_EXECUTOR_NAME)
  }
}

export function createJob({ jobName, jobFn } = {}) {
  return new PassthroughJob(jobName, jobFn)
}
