{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient } from '@prisma/client'

{=# prismaSetupFn.isDefined =}
const _waspSetupPrisma = {= prismaSetupFn.importIdentifier =}
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const _waspSetupPrisma = () => new PrismaClient()
{=/ prismaSetupFn.isDefined =}

function createDbClient(): PrismaClient {
  return _waspSetupPrisma()
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
export default dbClient
