import { sleep } from '../utils.js'

/**
 * Immutable passthrough job wrapper, mainly to be used for
 * testing.
 */
class PassthroughJob {
  constructor(perform) {
    this.perform = perform
    this.delayMs = 0
  }

  delay(ms) {
    const clonedJob = this._clone()
    clonedJob.delayMs = ms
    return clonedJob
  }

  performAsync(args) {
    const clonedJob = this._clone()
    return clonedJob._performAsync(args)
  }

  _clone() {
    const job = new PassthroughJob(this.perform)
    job.delayMs = this.delayMs
    return job
  }

  _performAsync(args) {
    async function fn(delayMs, perform) {
      if (delayMs > 0) {
        await sleep(delayMs)
      }
      return perform(args)
    }

    const res = fn(this.delayMs, this.perform)

    return {
      result: async () => await res
    }
  }
}

export default function(fn) {
  return new PassthroughJob(fn)
}
