import { sleep } from '../utils.js'
import { SubmittedJob } from './SubmittedJob.js'

export const PASSTHROUGH_EXECUTOR_NAME = Symbol('Passthrough')

/**
 * A simple job wrapper, mainly to be used for testing.
 */
class PassthroughJob {
  constructor(values) {
    this.jobFn = () => { }
    this.delaySeconds = 0
    this.jobName = 'N/A'
    Object.assign(this, values)
  }

  delay(delaySeconds) {
    return new PassthroughJob({ ...this, delaySeconds })
  }

  async submit(payload) {
    sleep(this.delaySeconds * 1000).then(() => this.jobFn(payload))
    // NOTE: Dumb random ID generator, mainly so we don't have to add `uuid`
    // as a dependency in the server generator for something nobody will likely use.
    let jobId = (Math.random() + 1).toString(36).substring(7)
    return new SubmittedJob(this.jobName, jobId, PASSTHROUGH_EXECUTOR_NAME)
  }
}

export async function executorSetup() { }

export function createJob({ jobName, jobFn } = {}) {
  return new PassthroughJob({ jobName, jobFn })
}
