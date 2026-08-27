{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}

function createDbClient() {
  return new InternalPrismaClient({=# isAuthEnabled =}{
    // The auth identity's secret material (password hashes, ...) never leaves
    // this column unless auth internals opt back in per query -- so it cannot
    // end up in an operation result or a log by accident.
    omit: {
      {= authIdentityEntityLower =}: {
        providerSecrets: true,
      },
    },
  }{=/ isAuthEnabled =})
}

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

// NOTE: If the app defines its own `prismaSetupFn`, the client it constructs is
// used as-is -- including any `omit` configuration it chooses (or omits).
export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', typeof createDbClient>;

{=# prismaSetupFn.isDefined =}
const dbClient: PrismaClient =  {= prismaSetupFn.importIdentifier =}();
{=/ prismaSetupFn.isDefined =}
{=^ prismaSetupFn.isDefined =}
const dbClient: PrismaClient = createDbClient();
{=/ prismaSetupFn.isDefined =}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
export type PrismaClient = null;

// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient: PrismaClient = null;
{=/ areThereAnyEntitiesDefined =}

// PUBLIC API
export default dbClient;
