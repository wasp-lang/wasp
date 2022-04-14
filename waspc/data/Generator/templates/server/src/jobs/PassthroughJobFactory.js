import { sleep } from '../utils.js'

/**
 * "Immutable-ish" passthrough job wrapper, mainly to be used for testing.
 */
class PassthroughJob {
  constructor(values) {
    this.perform = () => { }
    this.delaySeconds = 0
    Object.assign(this, values)
  }

  delay(delaySeconds) {
    return new PassthroughJob({ ...this, delaySeconds })
  }

  async performAsync(payload) {
    sleep(this.delaySeconds * 1000).then(() => this.perform(payload))
    return { jobType: 'passthrough' }
  }
}

export async function jobFactory(_jobName, fn, _options) {
  return new PassthroughJob({ perform: fn })
}
