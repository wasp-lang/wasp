{{={= =}=}}
declare module '{= prismaSetupFn.importPath =}' {
  import type { PrismaClient as InternalPrismaClient } from '@prisma/client'
  import type { FromRegistry } from 'wasp/types'

  type UserPrismaSetupFn = FromRegistry<'prismaSetupFn', () => InternalPrismaClient>;
  export const {= prismaSetupFn.exportName =}: UserPrismaSetupFn;
}
