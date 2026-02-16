type PrismaSetupFn = () => any

let _prismaSetupFn: PrismaSetupFn | undefined = undefined

export function registerPrismaSetupFn(fn: PrismaSetupFn): void {
  _prismaSetupFn = fn
}

export function getPrismaSetupFn(): PrismaSetupFn | undefined {
  return _prismaSetupFn
}
