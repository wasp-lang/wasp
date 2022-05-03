import { sleep } from '../../utils.js'
import { Job } from './Job.js'
import { SubmittedJob } from './SubmittedJob.js'

export const PASSTHROUGH_EXECUTOR_NAME = Symbol('Passthrough')

/**
 * A simple job mainly intended for testing. It will not submit work to any
 * job executor, but instead will simply invoke the underlying perform function.
 * It is dependency-free, however.
 */
class PassthroughJob extends Job {
  #jobFn
  #delaySeconds

  /**
   * 
   * @param {string} jobName - Name of the Job.
   * @param {*} jobFn - The Job function to execute.
   * @param {*} delaySeconds - The number of seconds to delay invoking the Job function.
   */
  constructor(jobName, jobFn, delaySeconds = 0) {
    super(jobName, PASSTHROUGH_EXECUTOR_NAME)
    this.#jobFn = jobFn
    this.#delaySeconds = delaySeconds
  }

  /**
   * @param {int} delaySeconds - Used to delay the processing of the job by some number of seconds.
   */
  delay(delaySeconds) {
    return new PassthroughJob(this.jobName, this.#jobFn, delaySeconds)
  }

  async submit(jobArgs) {
    sleep(this.#delaySeconds * 1000).then(() => this.#jobFn(jobArgs))
    // NOTE: Dumb random ID generator, mainly so we don't have to add `uuid`
    // as a dependency in the server generator for something nobody will likely use.
    let jobId = (Math.random() + 1).toString(36).substring(7)
    return new SubmittedJob(this, jobId)
  }
}

export function createJob({ jobName, jobFn } = {}) {
  return new PassthroughJob(jobName, jobFn)
}
