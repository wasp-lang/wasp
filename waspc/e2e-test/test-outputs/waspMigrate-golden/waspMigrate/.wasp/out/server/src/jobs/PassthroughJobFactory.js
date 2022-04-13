import { sleep } from '../utils.js'

/**
 * "Immutable-ish" passthrough job wrapper, mainly to be used for testing.
 */
class PassthroughJob {
  constructor(values) {
    this.perform = () => { }
    this.delayMs = 0
    Object.assign(this, values)
  }

  delay(ms) {
    return new PassthroughJob({ ...this, delayMs: ms })
  }

  performAsync(args) {
    return {
      result: sleep(this.delayMs).then(() => this.perform(args))
    }
  }
}

export function jobFactory(fn) {
  return new PassthroughJob({ perform: fn })
}
