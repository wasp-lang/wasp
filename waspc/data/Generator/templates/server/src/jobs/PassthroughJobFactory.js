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
    const clonedJob = new PassthroughJob({ ...this })
    return clonedJob._performAsync(args)
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

export default function (fn) {
  return new PassthroughJob({ perform: fn })
}
