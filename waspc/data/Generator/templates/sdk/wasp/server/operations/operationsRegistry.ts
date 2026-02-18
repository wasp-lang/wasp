type OperationFn = (...args: any[]) => any

const _operations = new Map<string, OperationFn>()

export function registerOperation(name: string, fn: OperationFn): void {
  _operations.set(name, fn)
}

// Returns a lazy wrapper that defers the registry lookup to invocation time.
//
// This is necessary because of a circular import during ESM initialization:
//   manifest.js → user operation code → SDK queries/index.js → getOperation()
//
// When queries/index.js evaluates, it calls getOperation() at module scope to
// build the wrapped operation exports. But manifest.js hasn't finished executing
// its registerOperation() calls yet (it's still resolving its own imports).
// The closure ensures the actual lookup happens at call time, when all
// registrations are guaranteed to be complete.
export function getOperation(name: string): OperationFn {
  return (...args: any[]) => {
    const fn = _operations.get(name)
    if (!fn) {
      throw new Error(`
        'Internal Wasp error:\nOperation '${name}' is not registered.`
      )
    }
    return fn(...args)
  }
}
