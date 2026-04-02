{{={= =}=}}
declare module '{= prismaSetupFn.importPath =}' {
  import type { PrismaClient as InternalPrismaClient } from '@prisma/client'
  import type { FromRegistry } from 'wasp/types'

  export const {= prismaSetupFn.exportName =}: FromRegistry<'prismaSetupFn', () => InternalPrismaClient>;
}
