type OperationFn = (...args: any[]) => any

const _operations = new Map<string, OperationFn>()

export function registerOperation(name: string, fn: OperationFn): void {
  _operations.set(name, fn)
}

export function getOperation(name: string): OperationFn {
  const fn = _operations.get(name)
  if (!fn) {
    throw new Error(`Operation '${name}' is not registered. This is a Wasp internal error.`)
  }
  return fn
}
