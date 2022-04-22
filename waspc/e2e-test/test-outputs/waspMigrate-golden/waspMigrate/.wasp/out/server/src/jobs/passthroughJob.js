import { sleep } from '../utils.js'

/**
 * "Immutable-ish" passthrough job wrapper, mainly to be used for testing.
 */
class PassthroughJob {
  constructor(values) {
    this.perform = () => { }
    this.delaySeconds = 0
    this.jobName = 'unknown'
    Object.assign(this, values)
  }

  delay(delaySeconds) {
    return new PassthroughJob({ ...this, delaySeconds })
  }

  async submit(payload) {
    sleep(this.delaySeconds * 1000).then(() => this.perform(payload))
    return {
      jobName: this.jobName,
      executor: 'passthrough',
      // NOTE: Dumb random ID generator, mainly so we don't have to pull uuid
      // as a dependency into the server generator for something nobody will likely use.
      jobId: (Math.random() + 1).toString(36).substring(7),
      passthrough: {}
    }
  }
}

export async function createJob(jobName, fn, _options) {
  return new PassthroughJob({ perform: fn, jobName })
}
