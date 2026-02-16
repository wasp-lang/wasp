{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import { getPrismaSetupFn } from './dbRegistry.js'
import type { Register } from 'wasp/types'

function createDbClient(): InternalPrismaClient {
  const prismaSetupFn = getPrismaSetupFn()
  if (prismaSetupFn) {
    return prismaSetupFn()
  }
  return new InternalPrismaClient()
}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
function createDbClient(): null {
  return null
}
{=/ areThereAnyEntitiesDefined =}

const dbClient = createDbClient()

// PUBLIC API
export type PrismaClient =
  Register extends { prismaSetupFn: infer T extends (...args: any[]) => any }
    ? ReturnType<T>
    : typeof dbClient

// PUBLIC API
export default dbClient as PrismaClient
