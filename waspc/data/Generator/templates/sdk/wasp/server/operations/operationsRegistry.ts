type OperationFn = (...args: any[]) => any

const _operations = new Map<string, OperationFn>()

export function registerOperation(name: string, fn: OperationFn): void {
  _operations.set(name, fn)
}

// Returns a lazy wrapper that defers the registry lookup until the operation
// is actually invoked. This is necessary because getOperation is called at
// module scope (during ES module initialization), but the manifest that
// registers operations may not have executed yet due to circular imports
// (manifest → user code → SDK operations → registry).
export function getOperation(name: string): OperationFn {
  return ((...args: any[]) => {
    const fn = _operations.get(name)
    if (!fn) {
      throw new Error(`Operation '${name}' is not registered. This is a Wasp internal error.`)
    }
    return fn(...args)
  })
}
