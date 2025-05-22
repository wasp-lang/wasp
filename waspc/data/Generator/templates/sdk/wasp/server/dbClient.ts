{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'

{=# prismaSetupFn.isDefined =}
const _waspSetUpPrisma = {= prismaSetupFn.importIdentifier =}
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const _waspSetUpPrisma = () => new InternalPrismaClient()
{=/ prismaSetupFn.isDefined =}

// We are not typing the return value because 
// we want to infer the type from the (potentially)
// user-defined function _waspSetUpPrisma
function createDbClient() {
  return _waspSetUpPrisma()
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
export type PrismaClient = typeof dbClient

// PUBLIC API
export default dbClient
