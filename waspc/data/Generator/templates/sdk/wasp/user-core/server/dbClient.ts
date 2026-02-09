{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}

function createDbClient() {
  return {= prismaSetupFn.importIdentifier =}()
}
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'

function createDbClient() {
  return new InternalPrismaClient()
}
{=/ prismaSetupFn.isDefined =}
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
export type PrismaClient = typeof dbClient

// PUBLIC API
export default dbClient
