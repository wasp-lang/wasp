import type * as z from 'zod'

export type ServerRuntimeBindings = {
  serverEnvValidationSchema?: z.ZodType
  prismaSetupFn?: () => unknown
  operations: Record<string, () => Promise<unknown>>
}

let serverRuntimeBindings: ServerRuntimeBindings | undefined

export function initializeServerRuntime(bindings: ServerRuntimeBindings): void {
  if (serverRuntimeBindings) {
    throw new Error('Wasp server runtime is already initialized')
  }

  serverRuntimeBindings = bindings
}

export function getServerEnvValidationSchema(): z.ZodType | undefined {
  if (!serverRuntimeBindings) {
    throw new Error(
      'Wasp server runtime is not initialized (while accessing server environment validation schema)',
    )
  }

  return serverRuntimeBindings.serverEnvValidationSchema
}

export function getServerPrismaSetupFn(): ServerRuntimeBindings['prismaSetupFn'] {
  if (!serverRuntimeBindings) {
    throw new Error(
      'Wasp server runtime is not initialized (while accessing Prisma setup function)',
    )
  }

  return serverRuntimeBindings.prismaSetupFn
}

export function getServerOperation<Operation>(name: string): Promise<Operation> {
  if (!serverRuntimeBindings) {
    throw new Error(`Wasp server runtime is not initialized (while accessing operation ${name})`)
  }

  const loadOperation = serverRuntimeBindings.operations[name]
  if (!loadOperation) {
    throw new Error(`Wasp server operation is not registered: ${name}`)
  }

  return loadOperation().then((operation) => operation as Operation)
}
