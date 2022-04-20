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
      jobId: 'unknown',
      passthrough: {}
    }
  }
}

export async function jobFactory(jobName, fn, _options) {
  return new PassthroughJob({ perform: fn, jobName })
}
